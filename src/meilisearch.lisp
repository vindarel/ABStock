
#+(or)
(ql:quickload '(log4cl shasht cl-ansi-term))

(defparameter *meilisearch-url* "http://127.0.0.1:7700"
  "Our MEILISEARCH_URL")

(defun generate-csv ()
  "Generate a CSV file from our query-all-cards query."
  (let ((query (sxql:yield (query-all-cards))))
    (uiop:run-program (str:replace-all "QUERY" query "sqlite3 -header -csv db.db 'QUERY' > test.csv")
                      :error-output t
                      )))

(defun document-add (&key (file "test.csv") (uid "books"))

  "Add a document.

  POST to /indexes/{index_uid}/documents

  HEADERS:
  - Content-type: text/csv

  PATH param:
  - index_uid: string

  When adding documents to an empty index, you can explicitly set the index's primary key as part of the document addition request.
  ?primaryKey=reference_number

  GET param:
  - csvDelimiter: defaults to , which is sqlite's default.

  BODY:
  - the csv content.

  debugging:
  - keys must be inserted in order and only one time => use newer version, like 1.12.1"
  (dex:post (str:concat *meilisearch-url* "/indexes/" uid "/documents"
                        "?primaryKey=id")
            :headers '(("Content-Type" . "text/csv"))
            :content (str:from-file file)))

    #|
    Example:
    curl \
    -X POST 'MEILISEARCH_URL/indexes/movies/documents' \
    -H 'Content-Type: application/json' \
    --data-binary '[
        {
        "id": 287947,
        "title": "Shazam",
        "poster": "https://image.tmdb.org/t/p/w1280/xnopI5Xtky18MPhK40cZAGAOVeV.jpg",
        "overview": "A boy is given the ability to become an adult superhero in times of need with a single magic word.",
        "release_date": "2019-03-23"
        }
    ]'
 |#

(defun document-get (&optional (uid "books"))
  "https://www.meilisearch.com/docs/reference/api/documents"
  (dex:get (str:concat *meilisearch-url* "/indexes/" uid "/documents/fetch")))

(defun ping ()
  (dex:get *meilisearch-url*))

#|
Update settings:

curl \
  -X PATCH 'MEILISEARCH_URL/indexes/movies/settings/typo-tolerance' \
  -H 'Content-Type: application/json' \
  --data-binary '{
    "minWordSizeForTypos": {
      "oneTypo": 4,
      "twoTypos": 10
    }
  }'
|#
(defun indexes-set-min-word-size-for-typos (&key (uid "books") (one-typo 4) (two-typos 6))
  "Typo tolerance happens by default at 5 characters (and two typos at 10.
  Set the min word size to 4, and 6 for two typos."
  (dex:patch (str:concat *meilisearch-url* "/indexes/" uid "/settings/typo-tolerance")
             :headers '(("Content-Type" . "application/json"))
             :content
             (shasht:write-json (dict "minWordSizeForTypos" (dict "oneTypo" one-typo
                                                                  "twoTypos" two-typos))
                                nil)
  ))

(defun set-setting (setting val &key (uid "books") raw dry-run)
  "Set this Meilisearch setting with a PATCH request to /indexes/[uid]/settings/[KEY]

  - setting: string
  - val: dict, to be transformed to JSON.

  Returns:
  - JSON with: the taskUid, the indexUid, the task status (enqueued), etc)
  - status
  - dict: content-length, content-type, cookies, date
  - URI hit: Quri object.

  Also possible:

  - enabled: true or false
  - disableOnWords: list of words
  - disableOnAttributes: list of attributes
  https://www.meilisearch.com/docs/learn/relevancy/typo_tolerance_settings"
  (let ((url (str:concat *meilisearch-url* "/indexes/" uid "/settings/" setting))
        (content (shasht:write-json val nil)))
    (log:debug "Calling URL " url "with" content)
    (when dry-run
      (return-from set-setting))
    (if raw
        (dex:patch url
                   :headers '(("Content-Type" . "application/json"))
                   :content content)
        (multiple-value-bind (json status res uri _)
            ;; (dex:patch url :headers '(("Content-Type" . "application/json")) :content content)
            (dex:put url :headers '(("Content-Type" . "application/json")) :content content)
          (values (shasht:read-json json)
                  status
                  res
                  uri
                  _)))))

#++
(set-setting "typo-tolerance" (dict "minWordSizeForTypos" (dict "oneTypo" 4 "twoTypos" 5)))

(defun set-filterable-attributes (&key (fields (list "title" "title_ascii"
                                                     "author" "author_ascii"
                                                     "author_bio"  ;; priority?
                                                     "summary"
                                                     "shelf" "shelf_id"
                                                     ))
                                    dry-run)
  (log:warn "we need dex:put for this setting")
  (set-setting "filterable-attributes" fields :dry-run dry-run))


(defun url/settings (&key (uid "books"))
  (str:concat *meilisearch-url* "/indexes/" uid "/settings"))

(defun settings (&key (uid "books"))
  (multiple-value-bind (json status res)
      (dex:get (url/settings :uid uid))
    (values
     (shasht:read-json json)
     status
     res)))

(defun get-sortable-attributes (&key (uid "books"))
  (dex:get (str:concat (url/settings :uid uid) "/sortable-attributes")))

(defun update-sortable-attributes (&optional (attrs (list "date_publication"))
                                                    &key (uid "books"))
  (dex:put (str:concat (url/settings :uid uid) "/sortable-attributes")
           :headers '(("Content-Type" . "application/json"))
           :content (shasht:write-json attrs nil)))

(defun table-settings (&rest args)
  (let ((settings (settings)))
    (term:vtable (apply #'access:accesses settings args))))

#++
(table-settings "typoTolerance" "minWordSizeForTypos")

(defun print-settings (settings &key (indentation 0) (column-size 25))
  (maphash (lambda (key val)
             (cond
               ((hash-table-p val)
                (format t "~a~a ->~&" (str:repeat indentation "  ") key)
                (print-settings (access:access settings key) :indentation (1+ indentation)))
               (t
                (format t "~a~va: ~va~&"
                        (str:repeat indentation "  ")  ;; add indentation
                        (- column-size (* 2 indentation))       ;; keep the : aligned at 20 chars
                        key
                        column-size   ;; the second ~va
                        val))))
           settings))

#++
(print-settings (settings))

(defun print-settings* (settings &rest keys)
  "Print a subset of settings by restricting them to a nested access of keys.

  Example:

  (print-settings* \"typoTolerance\" \"minWordSizeForTypos\")
  =>
  typoTolerance / minWordSizeForTypos
    oneTypo                : 4
    twoTypos               : 5"
  (format t "~a~&" (str:join " / " keys))
  (print-settings (apply #'access:accesses settings keys) :indentation 1))

(defun url/search (&key (uid "books"))
  (str:concat *meilisearch-url* "/indexes/" uid "/search"))

(defun post-request/json (url &key content)
  (dex:post url
            :headers '(("Content-Type" . "application/json"))
            :content content))

(defun document-search (q &key raw (uid "books")
                            (filter "")
                            (sort "date_publication")
                            (sort-order "desc")
                            (highlight (list "title" "author" "publisher")))
  "Search with query q.

  Body params:

  - offset
  - limit
  - page

  - filter: string
    filter by shelf or other field.
    We MUST have run (set-filterable-attributes) before.
    Use dot notation for nested fields.

  - attributesToHighlight: array of attributes or [*]
    Default: title, author fields.
    (Meilisearch default: null)
    > Highlights matching query terms in the specified attributes.
    This adds a field in each result: _formatted, the same dict result, where title and author have <em> </em> HTML tags to highlight the search terms.
    > By default highlighted elements are enclosed in <em> and </em> tags. You may change this by using the highlightPreTag and highlightPostTag search parameters.

  - sort: we MUST have run (update-sortable-attributes) which sets date_publication
   Meilisearch sorts results by: words, typo, proximity, attribute, sort and exactness.
   The sort fields are NOT a priority, so it isn't obvious to spot in the results.
   However, when searching NOTHING, then results are correctly sorted by publication date.
   >  Meilisearch will first place results closely matching search terms at the top of the returned documents list and only then will apply the sort parameters as requested by the user. In other words, by default Meilisearch provides a very relevant sorting.

  Returns: multiple values,
  - the first one is a dict:
    - hits: array of results
    - other fields for metadata: query, processing, limit, offset, estimatedTotalHits

  If raw is T, return raw Dexador results, withouth parsing the JSON results."
  (let ((url (url/search :uid uid))
        (content (shasht:write-json (dict
                                     "q" q
                                     "filter" filter
                                     ;; sort: ["date_publication:desc"]
                                     "sort" (list (format nil "~a:~a" sort sort-order))
                                     "attributesToHighlight" highlight
                                     )
                                    nil)))
    (if raw
        (post-request/json url :content content)
        (multiple-value-bind (json status res)
            (post-request/json url :content content)
          (values (shasht:read-json json)
                  status
                  res)))))

#++
;; rayon Asie 1 résultat
(document-search "akira" :filter "shelf_id = 1")

#++
;; rayon Sénégal 3 résultats
(document-search "akira" :filter "shelf_id = 4")

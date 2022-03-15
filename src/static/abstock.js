function get_cookie(name){
    // from leblason branch.
    return document.cookie.split(';').some(c => {
        return c.trim().startsWith(name + '=');
    });
}

function save_admin(textid) {
    // Get the editor element equal to the given textid.
    let editor = document.getElementById(textid);
    let content = editor.innerHTML;
    console.log("HTML to save is: ", content);

    let url = "/uuid-admin";
    if (textid) {
        url += "?textid=" + textid;
        console.log("POSTing on ", url);
    }

    fetch(url, {
        method: 'POST',
        body: content
    })
        .then((response) => {
            console.log("response is ", response);
            return response.json();
        })
        .then((myJson) => {
            if (myJson.status == 200 || myJson.status == "success") {
                console.log("-- success.");;
                Notiflix.Notify.Success('OK');
            }
            else {
                console.log("status is not success: ", myJson.status);
                Notiflix.Notify.Warning("OK ou pas ?");
            }
        })
        .catch((error) => {
            console.error('There has been a problem with your fetch operation:', error);
            Notiflix.Notify.Warning("An error occured. We have been notified.");
        });

}

console.log("loaded abstock .js");

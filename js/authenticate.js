function verify(form)
{
 if(form.userid.value == "teamaes" && 
  form.pswrd.value == "qwerty")
  {
    console.log("working");
    var statusText = "Ae$enCRypt0r!";
    document.getElementById("password").textContent = statusText;
    document.getElementById("reveal").textContent = "Your Password for this site is:";
    document.getElementById("authenticate").style.display = 'none';
    alert("The password for this site is " + statusText);
  }
 else
 {
   alert("Error Password or Username")
  }
}


// This function in the extension will callback the url of the open tab
// function getCurrentTabUrl(callback) {
//   var queryInfo = {
//     active: true,
//     currentWindow: true
//   };

//   chrome.tabs.query(queryInfo, function(tabs) {
//     var tab = tabs[0];
//     var url = tab.url;
//     console.assert(typeof url == 'string', 'tab.url should be a string');
//     verify(url); 
//   });


function check(url) {
  console.log(url);
  if(url.pwdcheck.value == "qwerty") {
    $.getJSON('data/vault.json', function(data) {
      console.log(data);
      var registered = false;
      data.map(function(d){
        if (d.url == url.urlcheck.value){
          registered = true;
          alert("The password for " + d.url + " is " + d.password);
          console.log("found");
        }
      });
      if (registered == false){
        alert("Website not registered");
      }
    });
  } else {
    alert("INCORRECT PASSWORD");
  }
}
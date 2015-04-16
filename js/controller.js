var masterpass = "qwerty";

$(document).ready(function(){
    // to test with ease
    $('#cancel1').click(function(){
      console.log("check");
      writejson();
    })

    $('#login').click(function(){
      console.log(this.form);
      verify(this.form);
    })

    $('#check').click(function(){
      check(this.form);
    })

    $('#add').click(function(){
      add(this.form);
    })
});

function verify(form)
{
 if(form.pswrd.value == masterpass)
  {
    alert("This can only be used in the extension");
    // getCurrentTabUrl();
  } else {
    window.location.replace("invalidpass.html");
  }
}


// This function in the extension will callback the url of the open tab
function getCurrentTabUrl() {
  var queryInfo = {
    active: true,
    currentWindow: true
  };

  chrome.tabs.query(queryInfo, function(tabs) {
    var tab = tabs[0];
    var url = tab.url;
    console.assert(typeof url == 'string', 'tab.url should be a string');
    retrieve(url);
  });

  // var url = "facebook.com"
  // retrieve(url);
}

function retrieve(url){
  console.log(url);
  $.getJSON('data/vault.json', function(data) {
      var registered = false;
      data.map(function(d){
        console.log(url.indexOf(d.url));
        if (url.indexOf(d.url) != -1){
          registered = true;
          // alert("The password for " + d.url + " is " + d.password);
          document.getElementById("password").textContent = d.password;
          document.getElementById("reveal").textContent = "Your Password for " + d.url + " is:";
          document.getElementById("authenticate").style.display = 'none';
        }
      });
      if (registered == false){
        window.location.replace("invalidurl.html");
      }
    });
}

function check(form) {
  if(form.pwdcheck.value == masterpass) {
    retrieve(form.urlcheck.value);
  } else {
    window.location.replace("invalidpass.html");
  }
}

function add(form) {
  if(form.pwdcheck.value == masterpass) {
    $.getJSON('data/vault.json', function(data) {

    });
  } else {
    window.location.replace("invalidpass.html");
  }
}

function writejson(){
  var dataObj = {};
  dataObj.highScore = 100000;
  dataObj.playerName = "Some Player";
  console.log(dataObj);
  localStorage.setItem("myKey", JSON.stringify(dataObj));
}
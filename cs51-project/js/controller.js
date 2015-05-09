var encryptedKey = "01100100 00001101 01010100 01100101 10111011 00110000 00010110 00111001 11110111 01010100 01111000 01111111 01111110 11000111 10011000 01101000 ";
var masterpass;

// catches the requests
$(document).ready(function(){
    // for congfucation. comment out when finished
    $('#cancel').click(function(){
      console.log("check");
      savestorage();
    })

    $('#login').click(function(){
      console.log(this.form);
      verify(this.form);
    })

    $('#check').click(function(){
      console.log("check");
      check(this.form);
    })

    $('#add').click(function(){
      add(this.form);
    })

    $('#encrypt').click(function(){
      encrypt(this.form, 1);
    })

    $('#decrypt').click(function(){
      encrypt(this.form, 0);
    })
});


// access current page
function verify(form) {
 masterpass = form.pswrd.value;
 if(form.pswrd.value.aes(form.pswrd.value, 1) == encryptedKey)
  {
    getCurrentTabUrl();
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


// retrive password from vault, then display on the page
function retrieve(url){
  console.log(url);
  chrome.storage.sync.get('vault', function(items) {
      var data = items.vault;
      var registered = false;
      data.map(function(d){
        console.log(url.indexOf(d.url));
        if (url.indexOf(d.url) != -1){
          registered = true;
          // alert("The password for " + d.url + " is " + d.password);
          document.getElementById("password").textContent = d.password.aes(masterpass, 0);
          document.getElementById("reveal").textContent = "Your Password for " + d.url + " is:";
          document.getElementById("authenticate").style.display = 'none';
        }
      });
      if (registered == false){
        window.location.replace("invalidurl.html");
      }
    });
}

// authenticate, then retrieve searched url
function check(form) {
  masterpass = form.pwdcheck.value;
  if(form.pwdcheck.value.aes(form.pwdcheck.value, 1) == encryptedKey) {
    retrieve(form.urlcheck.value);
  } else {
    window.location.replace("invalidpass.html");
  }
}

// add new password to vault
function add(form) {
  masterpass = form.mastpass.value;
  if(form.mastpass.value.aes(form.mastpass.value, 1) == encryptedKey) {
    chrome.storage.sync.get('vault', function(items){
      var data = items.vault;
      var registered = false;
      data.map(function(d){
        if(d.url == form.urladd.value){
          registered = true;
          d.password = form.pwdadd.value.aes(masterpass, 1),
          console.log(data);
          if (!chrome.runtime.error) {
              console.log('Settings saved');
               window.location.replace("updatesuccess.html");
            }
        }
      });
      if (registered == false){
        var addition = {
        "url": form.urladd.value,
        "password": form.pwdadd.value.aes(masterpass, 1)
        }
        data.push(addition);
        chrome.storage.sync.set({'vault': data}, function() {
            if (!chrome.runtime.error) {
              console.log('Settings saved');
               window.location.replace("addsuccess.html");
            }
          });
      };
    });
  } else {
    window.location.replace("invalidpass.html");
  }
}

// test helper function
function getvault(){
  chrome.storage.sync.get('vault', function(items){
    return items.vault;
  })
}

// test functions to configure chrome.storage
function savestorage(){
  var dataset = [];
  $.getJSON('data/vault_aesbinary.json', function(data) {
      console.log(data);
      dataset = data;
      console.log(dataset);
      chrome.storage.sync.set({'vault': dataset}, function() {
            // Notify that we saved.
            console.log('Settings saved');
          });
      document.getElementById("password").textContent = "Saved";
      getstorage();
  });
}

// test functions to configure chrome.storage
function getstorage(){
  chrome.storage.sync.get('vault', function(items) {
    if (!chrome.runtime.error) {
      document.getElementById("reveal").textContent = "Your default passwords have been";  
    }
    var data = [];
    data = items.vault;
    console.log(data);
  });
}

function encrypt(form, val) {
  console.log(form);
  if(form.key.value.length != 16) {
    alert("Key must be 16 Characters Long for Maximum Security")
  } else if (val == 1 && form.password.value.length > 16){
    alert("Password must be less than 16 characters")
  } else {
    var pass = form.password.value;
    var key = form.key.value;
    document.getElementById("password").textContent = pass.aes(key, val);
    document.getElementById("reveal").textContent = "Your Encrypted Source is:";
    document.getElementById("back").style.display = "block";
    document.getElementById("authenticate").style.display = 'none';
  }
}

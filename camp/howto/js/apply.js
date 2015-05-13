/**
 * ryu fellow
 * javascript.js
 * support camp.html
**/

var cnt = 0;
var ryus = new Array();
var selectImages = new Array();


/**
 * Function when top page is loaded.
**/
function setTopTitle() {

	setHeight();
	slideIn();

}

/**
 * Function when window resized.
 */
function whenResize() {

	setHeight();

}


/**
 * Set element height
**/
function setHeight() {
	var plus = 50;
	var bar = document.getElementById('bar');
	ryus[1] = document.getElementById('ryu-3');
	ryus[3] = document.getElementById('ryu-6');
	ryus[4] = document.getElementById('ryu-7');
	ryus[5] = document.getElementById('ryu-8');
	ryus[6] = document.getElementById('box-navi');
	ryus[7] = document.getElementById('box-caraban');
	ryus[8] = document.getElementById('box-camp');
	ryus[9] = document.getElementById('box-navi-hide');
	ryus[10] = document.getElementById('box-caraban-hide');
	ryus[11] = document.getElementById('box-camp-hide');
	ryus[12] = document.getElementById('ryu');
	ryus[13] = document.getElementById('bdy');
	ryus[14] = document.getElementById('nav');
	ryus[15] = document.getElementById('global-link');

	// mobile image
	ryus[16] = document.getElementById('mobile-image');


	var elementHeight = window.innerHeight / 2;
	var elementHeight2 = window.innerHeight / 3;
	var windowHeight = window.innerHeight;


	if (window.innerWidth < 500) { // window size less than 500px //
		ryus[1].style.width = "100%";
		ryus[3].style.width = "100%";
		ryus[4].style.width = "100%";
		ryus[5].style.width = "100%";

		// body background
		//ryus[13].style.backgroundSize = "auto " + windowHeight + "px";
		ryus[13].style.backgroundImage = "none";

		// global link
		ryus[15].style.display = "none";

		// mobile image
		ryus[16].style.display = "block";
		ryus[16].style.height = windowHeight - 10 + "px";

		bar.style.width = "100%";


	} else {                        // window size 500px or more //
		ryus[15].style.display = "block";
		ryus[16].style.display = "none";

		bar.style.width = "80%";

	}

	if (window.innerWidth < 1110) { // widow size less than 1110px //
		// body background
		ryus[13].style.backgroundSize = "auto 100%";
		ryus[13].style.backgroundPosition = "right";
	}

	if (window.innerWidth < 1375) { // window size less than 1375px //
		// navi
		ryus[3].style.backgroundSize = "auto 100%";
		// caravan
		ryus[4].style.backgroundSize = "auto 100%";
		// camp
		ryus[5].style.backgroundSize = "auto 100%";

		// navi hide
		ryus[6].style.backgroundSize = "auto 100%";
		// caravan hide
		ryus[7].style.backgroundSize = "auto 100%";
		// camp hide
		ryus[8].style.backgroundSize = "auto 100%";

	}

}


/**
 * Bar slide in when the page is loaded.
**/
function slideIn() {
	var fade_1 = 0.0;
	var slide = -8;
	selectImages[0] = document.getElementById('bar');

	var interval_fade1 = setInterval(function fade1() {
		selectImages[0].style.opacity = fade_1;
		fade_1 += 0.1;
		if (fade_1 > 1) {
			selectImages[0].style.opacity = 1;
			clearInterval(interval_fade1);
		}
	}, 20);
}

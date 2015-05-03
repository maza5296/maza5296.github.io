String.prototype.capitalizeFirstLetter = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
}

String.prototype.toTitleCase = function() {
    return this.replace(/\w\S*/g, function(txt){return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase();});
}

// Object.prototype.toArray = function() {
// 	var _Array = new Array();
//     for(var name in this) {
//            _Array[name] = this[name];
//    	}
//    	return _Array;
// };


function toArray(_Object){
       var _Array = new Array();
       for(var name in _Object){
               _Array[name] = _Object[name];
       }
       return _Array;
}
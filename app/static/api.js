
var getGame = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/game', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

var getGameByGameIdGameState = function(gameId, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/game/' + encodeURIComponent(gameId) + '/gameState', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

var postGameByGameIdGameState = function(gameId, body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/game/' + encodeURIComponent(gameId) + '/gameState', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

var getGameByGameIdBorders = function(gameId, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/game/' + encodeURIComponent(gameId) + '/borders', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

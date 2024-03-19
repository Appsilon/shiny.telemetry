Shiny.addCustomMessageHandler('setUserCookie', function(params) {
  Cookies.set(params.cookieName, params.cookieValue, {expires: params.expiryInDays, path: '/'});
});

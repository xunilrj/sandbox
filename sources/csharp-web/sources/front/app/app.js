import 'babel-polyfill';

import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { applyRouterMiddleware, Router, browserHistory } from 'react-router';
import { syncHistoryWithStore } from 'react-router-redux';
import { useScroll } from 'react-router-scroll';
import LanguageProvider from 'containers/LanguageProvider';
import configureStore from './store';

import { translationMessages } from './i18n';

import 'sanitize.css/sanitize.css';

const initialState = {};
const store = configureStore(initialState, browserHistory);

import { selectLocationState } from 'containers/App/selectors';
const history = syncHistoryWithStore(browserHistory, store, {
  selectLocationState: selectLocationState(),
});

import App from 'containers/App';
import createRoutes from './routes';
const rootRoute = {
  component: App,
  childRoutes: createRoutes(store),
};

const render = (translatedMessages) => {
  ReactDOM.render(
    <Provider store={store}>
      <LanguageProvider messages={translatedMessages}>
        <Router history={history} routes={rootRoute} render={applyRouterMiddleware(useScroll())} />
      </LanguageProvider>
    </Provider>,
    document.getElementById('app')
  );
};


// Hot reloadable translation json files
if (module.hot) {
  // modules.hot.accept does not accept dynamic dependencies,
  // have to be constants at compile-time
  module.hot.accept('./i18n', () => {
    render(translationMessages);
  });
}

// Chunked polyfill for browsers without Intl support
if (!window.Intl) {
  (new Promise((resolve) => {
    resolve(System.import('intl'));
  }))
    .then(() => Promise.all([
      System.import('intl/locale-data/jsonp/de.js'),
    ]))
    .then(() => render(translationMessages))
    .catch((err) => {
      throw err;
    });
} else {
  render(translationMessages);
}

// Install ServiceWorker and AppCache in the end since
// it's not most important operation and if main code fails,
// we do not want it installed
import { install } from 'offline-plugin/runtime';
install();

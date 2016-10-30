import CONSTANTS from './constants';

const initialState = {
  running: false,
};

function DependenEventsReducer(state = initialState, action) {
  switch (action.type) {
    case CONSTANTS.ACTIONSTART: return Object.assign({}, state, {
      running: true,
    });
    default:
      return state;
  }
}

DependenEventsReducer.InitialState = initialState;
export default DependenEventsReducer;

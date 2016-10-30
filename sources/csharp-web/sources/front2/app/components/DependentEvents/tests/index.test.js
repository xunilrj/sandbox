// import DependentEvents from '../index';

import expect from 'expect';
import { shallow } from 'enzyme';
import React from 'react';
import reducer from '../reducer';
import CONSTANTS from '../constants';

describe('DependentEventsReducer', () => {
  it('returns the initial state', () => {
    const initialState = reducer.InitialState;
    const result = reducer(undefined, {});

    expect(result).toEqual(initialState);
  });

  it('can start', () => {
    const initialState = reducer.InitialState;
    const result = reducer(initialState, {type:CONSTANTS.ACTIONSTART});

    expect(result).toEqual({running:true});
  });
});

describe('<DependentEvents />', () => {
  it('Expect to have unit tests specified', () => {
    expect(true).toEqual(true);
  });
});

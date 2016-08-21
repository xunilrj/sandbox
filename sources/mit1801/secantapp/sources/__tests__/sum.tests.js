jest.unmock('../libs/sum'); // unmock to use the actual implementation of sum
jest.unmock('react');
jest.unmock('react-test-renderer');

describe('sum', () => {
  it('adds 1 + 2 to equal 3', () => {
    const sum = require('../libs/sum');
    expect(sum(1, 2)).toBe(3);
  });

  it('renders correctly', () => {
    var React = require('react');
    var d = <div>Facebook</div>;
    //expect(tree).toMatchSnapshot();
  });
});
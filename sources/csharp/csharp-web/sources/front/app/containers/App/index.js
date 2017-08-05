import React from 'react';
import styles from './styles.css';

const App = (props) => (
  <div>
    <div className={styles.container}>
      {React.Children.toArray(props.children)}
    </div>
  </div>
);

App.propTypes = {
  children: React.PropTypes.node,
};

export default App;

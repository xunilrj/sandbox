import React from 'react';
import { Store, newStore, useNode, useInput, useInitialValue, mapToProps } from './react-freezerjs.js';

const Name2 = mapToProps(x => x.user, ({ name }) => {
    return <div>{name}</div>;
});

function Name(props) {
    console.log("render");

    useInitialValue(x => x.user, { name: "Daniel" });
    useInitialValue(x => x.users, []);

    const [user, setUser] = useNode(x => x.user);
    const name = useInput(x => x.name, user);

    const [users, push, splice] = useNode(x => x.users);
    let change = setUser({ name: 'Daniel2' });

    return <>
        <div>{user.name}</div>
        <button onClick={change}>Change Name</button>
        <div>
            <button onClick={push({})}>Add</button>
            <button onClick={splice(0, 1, [])}>Remove</button>
            <div>{JSON.stringify(users)}</div>
            <div>
                <input {...name}></input>
            </div>
        </div>
    </>
}

export default function HelloStore(props) {
    const store = newStore();
    // store.on('beforeAll', function (eventName, arg1, arg2) {
    //     console.log("beforeAll", eventName, arg1, arg2);
    // });
    return <Store value={store}>
        <Name />
        <Name2 />
    </Store>;
}
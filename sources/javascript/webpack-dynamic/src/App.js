import React, { Suspense } from 'react';
import ReactDOM from 'react-dom';
import { ErrorBoundary } from 'react-error-boundary'
import HelloStore from './HelloStore.js';

const HelloLazy = React.lazy(() => import('./Hello.js'));

function ErrorFallback({ error, componentStack, resetErrorBoundary }) {
    return (
        <div role="alert">
            <p>Something went wrong:</p>
            <pre>{error.message}</pre>
            <pre>{componentStack}</pre>
            <button onClick={resetErrorBoundary}>Try again</button>
        </div>
    )
}

function App() {
    const [explode, setExplode] = React.useState(false)
    return <>
        <ErrorBoundary FallbackComponent={ErrorFallback}
            onReset={() => setExplode(false)}
            resetKeys={[explode]}>
            <Suspense fallback={<div>loading...</div>}>
                <HelloLazy />
            </Suspense>
        </ErrorBoundary>
        <HelloStore />
    </>;
}

export function start() {
    ReactDOM.render(<App />,
        document.getElementById("app"));
}
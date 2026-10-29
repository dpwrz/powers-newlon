import React from 'react';
import {createRoot} from 'react-dom/client';
import {QueryClient,QueryClientProvider} from '@tanstack/react-query';
import {App} from './App';
import './styles.css';
const client=new QueryClient({defaultOptions:{queries:{retry:1,refetchOnWindowFocus:false,staleTime:300_000}}});
// Preserve the existing private Control Room entry while the consumer site
// transitions to Atlas. Admin operations remain isolated in /legacy/.
if(window.location.hash==='#/_fm/control-room-30'){
 window.location.replace('/legacy/'+window.location.hash);
}else createRoot(document.getElementById('root')!).render(<React.StrictMode><QueryClientProvider client={client}><App/></QueryClientProvider></React.StrictMode>);

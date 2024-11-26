import React, { useEffect, useState } from "react";
import axios from "axios";

import { createTaskList } from './tasks.js';
import Agenda from './Agenda.js';
import './App.css';


function App() {
    const [agendaTasks, setAgendaTasks] = useState([]);

    useEffect(() => {
        axios.get("agenda.json").then((res) => {
            setAgendaTasks(createTaskList(res.data.tasks));
        });
    }, []);
    
    return (
        <div className="App">
            <Agenda tasks={agendaTasks} />
        </div>
    );
}

export default App;

import React, { useEffect, useState } from "react";
import axios from "axios";

import Button from 'react-bootstrap/Button';
import Form from 'react-bootstrap/Form';
import Nav from 'react-bootstrap/Nav';
import Tab from 'react-bootstrap/Tab';

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
            <Tab.Container defaultActiveKey="agenda">
                <Nav variant="tabs">
                    <Nav.Item>
                        <Nav.Link eventKey="agenda">Agenda</Nav.Link>
                    </Nav.Item>
                    <Nav.Item>
                        <Nav.Link eventKey="zetian">Zetian</Nav.Link>
                    </Nav.Item>
                    <Nav.Item className="ms-auto">
                        <Form inline="true">
                            <Form.Control
                                id="search"
                                type="text"
                                placeholder="Search"
                            />
                        </Form>
                    </Nav.Item>
                </Nav>
                <Tab.Content>
                    <Tab.Pane eventKey="agenda">
                        <Agenda tasks={agendaTasks} />
                    </Tab.Pane>
                    <Tab.Pane eventKey="zetian">
                        Notes view
                    </Tab.Pane>
                </Tab.Content>
            </Tab.Container>
        </div>
    );
}

export default App;

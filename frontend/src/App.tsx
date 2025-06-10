import { Routes, Route, BrowserRouter } from "react-router-dom";
import { ToastContainer } from "react-toastify";

import "./css/global_tailwind.css";

import MonacoEditor from "./Editor";
import Home from "./Home";
import NavBar from "./NavBar";
import { AuthProvider } from "./AuthContext";

import NotFound from "./NotFound";

const App = () => {
    return (
        <AuthProvider>
            <BrowserRouter>
                <Layout>
                    <Routes>
                        <Route path="/" element={<Home />} />
                        <Route path="/home" element={<Home />} />
                        <Route
                            path="/about"
                            element={<>ABout me...i'll tell you soon...</>}
                        />
                        <Route path="/editor" element={<MonacoEditor />} />
                        <Route
                            path="/documentation"
                            element={
                                <>
                                    HAHAH FUCK YOU I DONT GET PAID TO WRITE
                                    Documentation
                                </>
                            }
                        />
                        <Route
                            path="/collaborate"
                            element={<>Hello you wanna collab deez nuts?</>}
                        />
                        <Route path="*" element={<NotFound />} />
                    </Routes>
                </Layout>
            </BrowserRouter>
        </AuthProvider>
    );
};

const Layout = ({ children }: { children: React.ReactNode }) => {
    return (
        <>
            <ToastContainer pauseOnHover={false} />
            <NavBar />
            <main>{children}</main>
        </>
    );
};

export default App;


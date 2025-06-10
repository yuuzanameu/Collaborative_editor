import { useLocation } from "react-router-dom";
import styles from "./css/NavBar.module.css";
import { Link } from "react-router-dom";
import logo from "./resources/edit.png";

import { LoggedOutButtons, LoggedinAccount } from "./Signup_Login/Login";
import { useAuth } from "./AuthContext";

function NavBar() {
    const { signedIn } = useAuth();
    const location = useLocation();
    // console.log("signed in status", signedIn);

    const getNavbarStyle = () => {
        switch (location.pathname) {
            case "/about":
                return { backgroundColor: "blue" };
            case "/editor":
                return { backgroundColor: "green" };
            case "/documentation":
                return { backgroundColor: "purple" };
            default:
                return {};
        }
    };

    return (
        <nav className={styles.navbar} style={getNavbarStyle()}>
            <nav
                className={`${styles.navItems} ${styles.navlinkfonts} ${styles.navLinkItems}`}
            >
                <img src={logo} alt="App Logo" className={styles.logo} />
                {[
                    ["/home", "Home"],
                    ["/about", "About"],
                    ["/documentation", "Documentation"],
                ].map(([link, linkcontent]) => {
                    const understyle = `${styles.underlined}`;
                    return (
                        <Link
                            key={link}
                            to={link}
                            className={`${location.pathname === link ? understyle : ""}`}
                        >
                            {linkcontent}
                        </Link>
                    );
                })}
            </nav>
            {signedIn ? <LoggedinAccount /> : <LoggedOutButtons />}
        </nav>
    );
}

export default NavBar;

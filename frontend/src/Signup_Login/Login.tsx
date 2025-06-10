import React, { createContext, useContext, useEffect, useState } from "react";
import ReactDOM from "react-dom";
import { toast } from "react-toastify";

// css import
import styles from "../css/NavBar.module.css";
import signupstyles from "../css/Signin/SignUp.module.css";
import loginstyles from "../css/Signin/Login.module.css";
import commonstyles from "../css/Signin/Common.module.css";

// image import
import userIcon from "../resources/user.png";
import lock from "../resources/lock.png";
import accountIcon from "../resources/account.png";
import spinner from "../resources/spinner.svg";

//react component import
import CloseIcon from "./CloseIcon";
import { useAuth } from "../AuthContext";
import { useFeedback } from "./Feedback";
import ToastTemplate from "./Toast_Template";

type AuthFormData = {
    email: string;
    password: string;
};

type FormContextType = {
    signupData: AuthFormData;
    loginData: AuthFormData;
    updateSignupData: (data: AuthFormData) => void;
    updateLoginData: (data: AuthFormData) => void;
};

const FormContext = createContext<FormContextType>({
    signupData: { email: "", password: "" },
    loginData: { email: "", password: "" },
    updateSignupData: (_) => {},
    updateLoginData: (_) => {},
});

export const LoggedOutButtons = () => {
    const [SignupClicked, setSignupClicked] = useState(false);
    const [LoginClicked, setLoginClicked] = useState(false);
    const [signupData, setSignupData] = useState<AuthFormData>({
        email: "",
        password: "",
    });
    const [loginData, setLoginData] = useState<AuthFormData>({
        email: "",
        password: "",
    });

    const { loading } = useAuth();

    function updateSignupData(data: AuthFormData) {
        setSignupData(data);
    }
    function updateLoginData(data: AuthFormData) {
        setLoginData(data);
    }

    function showSignupWindow() {
        setSignupClicked(true);
    }
    function showLoginWindow() {
        setLoginClicked(true);
    }

    function closeSigninWindow() {
        setLoginClicked(false);
        setSignupClicked(false);
    }

    return (
        <FormContext.Provider
            value={{ signupData, loginData, updateSignupData, updateLoginData }}
        >
            <nav className={`${styles.navItems} ${commonstyles.loginButtons}`}>
                <button onClick={showSignupWindow}>Sign up</button>
                <button onClick={showLoginWindow}>Log in</button>
            </nav>
            {SignupClicked && (
                <OverlayPortal>
                    <SignupWindow closeWindow={closeSigninWindow} />
                </OverlayPortal>
            )}
            {LoginClicked && (
                <OverlayPortal>
                    <LoginWindow closeWindow={closeSigninWindow} />
                </OverlayPortal>
            )}
            {loading && (
                <OverlayPortal>
                    <LoadingScreen />
                </OverlayPortal>
            )}
        </FormContext.Provider>
    );
};

// PORTALS

const loading_overlays: Element | DocumentFragment =
    document.getElementById("loading_overlays") ||
    document.createElement("fallbackLoadingOverlay");

const overlays: Element | DocumentFragment =
    document.getElementById("overlays") ||
    document.createElement("fallbackOverlay");

const LoadingOverlayPortal = ({ children }: { children: React.ReactNode }) => {
    return ReactDOM.createPortal(children, loading_overlays);
};

const OverlayPortal = ({ children }: { children: React.ReactNode }) => {
    return ReactDOM.createPortal(children, overlays);
};

const LoadingScreen = () => {
    return (
        <div className={commonstyles.loading_overlay}>
            <img src={spinner} />
        </div>
    );
};

type Props = {
    closeWindow: () => void;
};

const CloseX: React.FC<Props> = ({ closeWindow }) => {
    return (
        <div onClick={closeWindow}>
            <CloseIcon />
        </div>
    );
};

async function addUserToDatabase(email: string, password: string) {
    const options = {
        headers: {
            "Content-type": "application/json",
        },
        body: JSON.stringify({ email, password }),
        method: "POST",
    };

    try {
        const res = await fetch("/api/auth/sign-up/", options);

        if (!res.ok) {
            const errorData = await res.json();
            throw errorData;
        }
        console.log(await res.json());
        console.log("Successful signup");
        return { success: true, error: "" };
    } catch (err) {
        console.error("Error: api/auth/sign-up", err);
        return { success: false, error: err };
    }
}

function semantic_check(e_mail: string, password: string) {
    let emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    const res = { semanticError: true, message: "" };

    if (e_mail === "" || password === "") {
        res.message = "Email or password is empty";
    } else if (e_mail.includes(" ")) {
        res.message = "Invalid Email: contains whitespace";
    } else if (!e_mail.includes("@")) {
        res.message = "Invalid Email: doesnt contain @";
    } else if (!emailRegex.test(e_mail)) {
        res.message = "Invalid email";
    } else if (password.length < 6) {
        res.message = "Password should be at least 6 characters long";
    } else {
        res.semanticError = false;
    }

    return res;
}

const SignupForm = () => {
    const { signupData, updateSignupData } = useContext(FormContext);
    const { showError, clearFeedback, FeedbackComponent } = useFeedback();
    const customId = "signup";
    const { showLoading, loading } = useAuth();

    function handleSignup() {
        validateUserDetails(signupData.email, signupData.password);
        // showLoading(true);
    }

    async function validateUserDetails(e_mail: string, password: string) {
        const res = semantic_check(e_mail, password);
        if (res.semanticError) {
            // this catches simple semantic errors while entering
            // user credentials
            showError(res.message);
        } else {
            // if all good call to add new email and password
            // to database is executed, this may fail if entered email
            // doesnt actually exist or is just unreachable for some reason
            // or some other internal server error related to mail service provider
            showLoading(true);
            const res = await addUserToDatabase(e_mail, password);
            function sleep(ms: number) {
                return new Promise((resolve) => setTimeout(resolve, ms));
            }
            // const res = { success: true, error: "" };
            // await sleep(3000);
            showLoading(false);

            if (!res.success) {
                switch (res.error) {
                    case "AccountAlreadyExists":
                        showError(
                            "Your account is already registered, please log in.",
                        );
                        break;
                    case "VerificationPending":
                        showError(
                            "Verification email sent, please check email.",
                        );
                        break;
                    case "EmailUnreachable":
                        showError(
                            "Your email is unreachable, check if it's the right one.",
                        );
                        break;
                    default:
                        showError("Something went wrong, try again later.");
                        break;
                }
            } else {
                clearFeedback();
                updateSignupData({ email: "", password: "" });
                toast.success(<ToastTemplate e_mail={e_mail} />, {
                    position: "top-center",
                    toastId: customId,
                    autoClose: 1000 * 10,
                });
            }
        }
    }

    return (
        <div className={`${signupstyles.signupform}`}>
            <FeedbackComponent />
            <div className={commonstyles.formInputBox}>
                <img
                    src={userIcon}
                    alt="user"
                    className={commonstyles.signinicons}
                />
                <input
                    type="email"
                    value={signupData.email}
                    onChange={(e) =>
                        updateSignupData({
                            ...signupData,
                            email: e.target.value,
                        })
                    }
                    placeholder="E-mail"
                />
            </div>
            <div className={commonstyles.formInputBox}>
                <img
                    src={lock}
                    alt="password"
                    className={commonstyles.signinicons}
                />
                <input
                    type="password"
                    value={signupData.password}
                    onChange={(e) =>
                        updateSignupData({
                            ...signupData,
                            password: e.target.value,
                        })
                    }
                    placeholder="Password"
                />
            </div>
            <button
                className={commonstyles.signupSubmit}
                type="submit"
                onClick={handleSignup}
            >
                Sign up
            </button>
        </div>
    );
};

const SignupWindow: React.FC<Props> = ({ closeWindow }) => {
    return (
        <div className={commonstyles.overlay} onClick={closeWindow}>
            <div
                className={signupstyles.modal}
                onClick={(e) => e.stopPropagation()}
            >
                <section
                    className={`${signupstyles.modalHeader} ${commonstyles.fira_sans_semibold}`}
                >
                    <h1>Sign up</h1>
                    <CloseX closeWindow={closeWindow} />
                </section>

                <SignupForm />

                <section className={signupstyles.modalsubtitle}>
                    <p>You are creating a new account</p>
                </section>
            </div>
        </div>
    );
};

const LoginWindow: React.FC<Props> = ({ closeWindow }) => {
    return (
        <div className={commonstyles.overlay} onClick={closeWindow}>
            <div
                className={loginstyles.modal}
                onClick={(e) => e.stopPropagation()}
            >
                <section
                    className={`${loginstyles.modalHeader} ${commonstyles.fira_sans_semibold}`}
                >
                    <h1>Login</h1>
                    <CloseX closeWindow={closeWindow} />
                </section>

                <LoginForm />
            </div>
        </div>
    );
};

const LoginForm = () => {
    const { setAuthorised } = useAuth();
    const [email, setEmail] = useState("");
    const [pswd, setPswd] = useState("");

    const { showError, showSuccess, clearFeedback, FeedbackComponent } =
        useFeedback();

    async function attemptLogin(emailID: string, password: string) {
        const opts = {
            headers: {
                "Content-type": "application/json",
            },
            body: JSON.stringify({ email: emailID, password: password }),
            method: "POST",
        };
        try {
            const res = await fetch("/api/auth/login/", opts);
            if (!res.ok) {
                const data = await res.json();
                throw data;
            }
            const data = await res.json();
            console.log(data.userProfile);
            setAuthorised(true);
        } catch (err) {
            switch (err) {
                case "NoSuchEmail":
                    showError("Your email is not registered, please sign up.");
                    break;
                case "PasswordMismatch":
                    showError("Email id and password don't match.");
                    break;
                default:
                    showError("Something went wrong, try again later.");
                    break;
            }
        }
    }

    async function validateLoginDetails(e_mail: string, password: string) {
        const res = semantic_check(e_mail, password);
        if (res.semanticError) {
            showError(res.message);
        } else {
            await attemptLogin(e_mail, password);
        }
    }

    return (
        <div className={loginstyles.loginform}>
            <FeedbackComponent />
            <div className={commonstyles.formInputBox}>
                <img
                    src={userIcon}
                    alt="user"
                    className={commonstyles.signinicons}
                />
                <input
                    type="email"
                    value={email}
                    onChange={(e) => setEmail(e.target.value)}
                    placeholder="E-mail or Username"
                />
            </div>
            <div className={commonstyles.formInputBox}>
                <img
                    src={lock}
                    alt="password"
                    className={commonstyles.signinicons}
                />
                <input
                    type="password"
                    value={pswd}
                    onChange={(e) => setPswd(e.target.value)}
                    placeholder="Password"
                />
            </div>
            <div className={loginstyles.remembermePswd}>
                <div className={loginstyles.rememberme}>
                    <input type="checkbox" />
                    <p>Remember me</p>
                </div>
                <p className={loginstyles.forgotpswd}>Forgot password?</p>
            </div>
            <button
                className={commonstyles.signupSubmit}
                type="submit"
                onClick={() => validateLoginDetails(email, pswd)}
            >
                Log in
            </button>
        </div>
    );
};

export function LoggedinAccount() {
    return (
        <div className="grid place-items-center w-15 border-white border-2">
            <img
                src={accountIcon}
                alt="account"
                className="w-8 h-8 cursor-pointer"
            />
        </div>
    );
}

// Todo
// Implement loading screen after you sucesffuly finish signup or login request
// use hooks like useMemo useReducer

import { createContext, useState, useContext, useEffect } from "react";

type AuthContextType = {
    signedIn: boolean;
    setAuthorised: (user: any) => void;
    loading: boolean;
    showLoading: (loadingState: boolean) => void;
};

const AuthContext = createContext<AuthContextType>({
    signedIn: false,
    setAuthorised: () => {},
    loading: false,
    showLoading: () => {},
});

export function useAuth() {
    return useContext(AuthContext);
}

export const AuthProvider: React.FC<{ children: React.ReactNode }> = ({
    children,
}) => {
    const [signedIn, setSignedIn] = useState(false);
    const [loading, setLoading] = useState(false);

    function setAuthorised(status: boolean) {
        setSignedIn(status);
    }

    function showLoading(loadingState: boolean) {
        setLoading(loadingState);
    }

    useEffect(() => {
        async function validateUserCookies() {
            try {
                const res = await fetch("/api/auth");
                if (!res.ok) {
                    const data = await res.json();
                    throw data;
                }
                const data = await res.json();
                console.log("sucess: ", data);
                setAuthorised(true);
            }
            catch(e){
                console.log("error: ", e)
                setAuthorised(false);
            }
        }

        validateUserCookies();
    }, [signedIn]);

    return (
        <>
            <AuthContext.Provider
                value={{ signedIn, setAuthorised, loading, showLoading }}
            >
                {children}
            </AuthContext.Provider>
        </>
    );
};

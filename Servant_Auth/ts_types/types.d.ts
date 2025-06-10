type Credentials = ICredentials;

interface ICredentials {
  email: string;
  password: string;
}

type SignUpErrors =
  | "AccountAlreadyExists"
  | "VerificationPending"
  | "EmailUnreachable";

type Profile = IProfile;

interface IProfile {
  userName: string;
  profilePic: string;
}

type LoginState = ILoginState;

interface ILoginState {
  loggedin: boolean;
}

type LoginErrors = "NoSuchEmail" | "PasswordMismatch";

type LoginResponse = ILoggedIn;

interface ILoggedIn {
  userProfile: Profile;
}


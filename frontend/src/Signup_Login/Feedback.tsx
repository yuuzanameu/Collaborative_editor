import { useState } from 'react';

import styles from '../css/Signin/Common.module.css'

type FeedbackType = 'error' | 'success' | 'info' | 'warning';

type FeedbackState = {
    message: string;
    type: FeedbackType;
    isVisible: boolean;
}

interface UseFeedbackReturn {
    feedback: FeedbackState;
    showError: (message: string) => void;
    showSuccess: (message: string) => void;
    showInfo: (message: string) => void;
    showWarning: (message: string) => void;
    clearFeedback: () => void;
    FeedbackComponent: React.FC;
}

export const useFeedback = (): UseFeedbackReturn => {
    const [feedback, setFeedback] = useState<FeedbackState>({
        message: '',
        type: 'info',
        isVisible: false,
    });

    const showFeedback = (message: string, type: FeedbackType) => {
        setFeedback({
            message,
            type,
            isVisible: true,
        });
    };

    const showError = (message: string) => showFeedback(message, 'error');
    const showSuccess = (message: string) => showFeedback(message, 'success');
    const showInfo = (message: string) => showFeedback(message, 'info');
    const showWarning = (message: string) => showFeedback(message, 'warning');

    const clearFeedback = () => {
        setFeedback(prev => ({ ...prev, isVisible: false }));
    };

    // Inline component with styling based on feedback type
    const FeedbackComponent: React.FC = () => {
        if (!feedback.isVisible) return null;

        const typeStyles = {
            error: "text-[#FF4141] border-2 border-[#FF4141]",
            success: "text-green-400 border-2 border-green-400",
            info: "bg-blue-100 text-blue-800 border border-blue-200",
            warning: "bg-yellow-100 text-yellow-800 border border-yellow-200"
        };

        return (
            <div className={`${styles.baseFeedback} ${typeStyles[feedback.type]}`}>
                <p>{feedback.message}</p>
            </div>
        );
    };

    return {
        feedback,
        showError,
        showSuccess,
        showInfo,
        showWarning,
        clearFeedback,
        FeedbackComponent
    };
};

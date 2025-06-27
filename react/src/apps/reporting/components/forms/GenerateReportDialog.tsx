import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '@/components/ui/dialog';
import { FileText, Loader2, TrendingUp, Users } from 'lucide-react';

/**
 * GenerateReportDialog component props
 */
interface GenerateReportDialogProps {
    /** Whether the dialog is open */
    isOpen: boolean;
    /** Callback when dialog should close */
    onClose: () => void;
    /** Callback when report is successfully generated */
    onSuccess?: () => void;
}

/**
 * GenerateReportDialog Component
 */
export function GenerateReportDialog({ isOpen, onClose, onSuccess }: GenerateReportDialogProps) {
    const [reportType, setReportType] = useState('financial');
    const [isGenerating, setIsGenerating] = useState(false);

    const handleGenerate = async () => {
        setIsGenerating(true);

        // Simulate report generation
        await new Promise(resolve => setTimeout(resolve, 2000));

        setIsGenerating(false);
        onSuccess?.();
        onClose();
    };

    return (
        <Dialog open={isOpen} onOpenChange={onClose}>
            <DialogContent className="max-w-md">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <FileText className="h-5 w-5" />
                        Generate New Report
                    </DialogTitle>
                    <DialogDescription>
                        Generate comprehensive reports and analytics for your business insights.
                    </DialogDescription>
                </DialogHeader>

                <div className="space-y-4">
                    <div>
                        <label className="text-sm font-medium">Report Type</label>
                        <Select value={reportType} onValueChange={setReportType}>
                            <SelectTrigger className="mt-2">
                                <SelectValue placeholder="Select report type" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="financial">
                                    <div className="flex items-center gap-2">
                                        <FileText className="h-4 w-4" />
                                        Financial Report
                                    </div>
                                </SelectItem>
                                <SelectItem value="sales">
                                    <div className="flex items-center gap-2">
                                        <TrendingUp className="h-4 w-4" />
                                        Sales Analytics
                                    </div>
                                </SelectItem>
                                <SelectItem value="performance">
                                    <div className="flex items-center gap-2">
                                        <Users className="h-4 w-4" />
                                        Employee Performance
                                    </div>
                                </SelectItem>
                            </SelectContent>
                        </Select>
                    </div>

                    <div className="grid grid-cols-2 gap-4">
                        <div>
                            <label className="text-sm font-medium">Start Date</label>
                            <input
                                type="date"
                                className="mt-2 flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                                defaultValue={new Date(new Date().getFullYear(), new Date().getMonth(), 1).toISOString().split('T')[0]}
                            />
                        </div>
                        <div>
                            <label className="text-sm font-medium">End Date</label>
                            <input
                                type="date"
                                className="mt-2 flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                                defaultValue={new Date().toISOString().split('T')[0]}
                            />
                        </div>
                    </div>
                </div>

                <DialogFooter>
                    <Button variant="outline" onClick={onClose} disabled={isGenerating}>
                        Cancel
                    </Button>
                    <Button onClick={handleGenerate} disabled={isGenerating}>
                        {isGenerating ? (
                            <>
                                <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                                Generating...
                            </>
                        ) : (
                            'Generate Report'
                        )}
                    </Button>
                </DialogFooter>
            </DialogContent>
        </Dialog>
    );
} 
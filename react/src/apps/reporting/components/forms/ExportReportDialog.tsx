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
import { Download, Loader2, FileText, Table, File } from 'lucide-react';

/**
 * ExportReportDialog component props
 */
interface ExportReportDialogProps {
    /** Whether the dialog is open */
    isOpen: boolean;
    /** Callback when dialog should close */
    onClose: () => void;
    /** Callback when report is successfully exported */
    onSuccess?: () => void;
}

/**
 * ExportReportDialog Component
 */
export function ExportReportDialog({ isOpen, onClose, onSuccess }: ExportReportDialogProps) {
    const [exportFormat, setExportFormat] = useState('pdf');
    const [isExporting, setIsExporting] = useState(false);

    const handleExport = async () => {
        setIsExporting(true);

        // Simulate export process
        await new Promise(resolve => setTimeout(resolve, 3000));

        setIsExporting(false);
        onSuccess?.();
        onClose();
    };

    const getFormatIcon = (format: string) => {
        switch (format) {
            case 'pdf':
                return <FileText className="h-4 w-4" />;
            case 'excel':
                return <Table className="h-4 w-4" />;
            case 'csv':
                return <File className="h-4 w-4" />;
            default:
                return <Download className="h-4 w-4" />;
        }
    };

    return (
        <Dialog open={isOpen} onOpenChange={onClose}>
            <DialogContent className="max-w-md">
                <DialogHeader>
                    <DialogTitle className="flex items-center gap-2">
                        <Download className="h-5 w-5" />
                        Export Report
                    </DialogTitle>
                    <DialogDescription>
                        Export your report in your preferred format for sharing or archiving.
                    </DialogDescription>
                </DialogHeader>

                <div className="space-y-4">
                    <div>
                        <label className="text-sm font-medium">Export Format</label>
                        <Select value={exportFormat} onValueChange={setExportFormat}>
                            <SelectTrigger className="mt-2">
                                <SelectValue placeholder="Select export format" />
                            </SelectTrigger>
                            <SelectContent>
                                <SelectItem value="pdf">
                                    <div className="flex items-center gap-2">
                                        <FileText className="h-4 w-4" />
                                        PDF Document
                                    </div>
                                </SelectItem>
                                <SelectItem value="excel">
                                    <div className="flex items-center gap-2">
                                        <Table className="h-4 w-4" />
                                        Excel Spreadsheet
                                    </div>
                                </SelectItem>
                                <SelectItem value="csv">
                                    <div className="flex items-center gap-2">
                                        <File className="h-4 w-4" />
                                        CSV File
                                    </div>
                                </SelectItem>
                            </SelectContent>
                        </Select>
                    </div>

                    <div className="flex items-center space-x-2">
                        <input
                            type="checkbox"
                            id="includeCharts"
                            className="h-4 w-4"
                            defaultChecked
                        />
                        <label htmlFor="includeCharts" className="text-sm font-medium">
                            Include charts and graphs
                        </label>
                    </div>

                    <div className="flex items-center space-x-2">
                        <input
                            type="checkbox"
                            id="includeRawData"
                            className="h-4 w-4"
                        />
                        <label htmlFor="includeRawData" className="text-sm font-medium">
                            Include raw data tables
                        </label>
                    </div>
                </div>

                <DialogFooter>
                    <Button variant="outline" onClick={onClose} disabled={isExporting}>
                        Cancel
                    </Button>
                    <Button onClick={handleExport} disabled={isExporting}>
                        {isExporting ? (
                            <>
                                <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                                Exporting...
                            </>
                        ) : (
                            <>
                                {getFormatIcon(exportFormat)}
                                <span className="ml-2">Export as {exportFormat.toUpperCase()}</span>
                            </>
                        )}
                    </Button>
                </DialogFooter>
            </DialogContent>
        </Dialog>
    );
}
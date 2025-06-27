import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import {
    BarChart3,
    FileText,
    TrendingUp,
    Users,
    Calendar,
    Download,
    Plus,
    DollarSign,
    Target,
    Activity,
    AlertCircle
} from 'lucide-react';

import { FinancialReports } from '../components/reports/FinancialReports';
import { SalesAnalytics } from '../components/analytics/SalesAnalytics';
import { EmployeePerformance } from '../components/performance/EmployeePerformance';
import { ReportDashboard } from '../components/dashboard/ReportDashboard';
import { GenerateReportDialog } from '../components/forms/GenerateReportDialog';
import { ExportReportDialog } from '../components/forms/ExportReportDialog';
import { useReporting, useReportDashboard, useFinancialReports, useSalesAnalytics } from '../hooks/use-reporting';

/**
 * ReportingInterface Component
 * 
 * Main reporting and analytics interface following the established design pattern.
 * Multi-tab layout with financial reports, sales analytics, employee performance, and dashboard.
 */
export function ReportingInterface() {
    const [activeTab, setActiveTab] = useState('dashboard');
    const [showGenerateDialog, setShowGenerateDialog] = useState(false);
    const [showExportDialog, setShowExportDialog] = useState(false);

    const { hasActiveReport, isGenerating, isExporting } = useReporting();
    const { dashboardMetrics, totalReports } = useReportDashboard();
    const { totalReports: financialCount } = useFinancialReports();
    const { totalAnalytics: salesCount } = useSalesAnalytics();

    const handleGenerateReport = () => {
        setShowGenerateDialog(true);
    };

    const handleExportReport = () => {
        if (hasActiveReport) {
            setShowExportDialog(true);
        }
    };

    const handleReportGenerated = () => {
        setShowGenerateDialog(false);
    };

    const handleReportExported = () => {
        setShowExportDialog(false);
    };

    return (
        <div className="h-full bg-background">
            {/* Main Content */}
            <div className="flex h-full">
                {/* Left Side - Main Content */}
                <div className="flex-1 flex flex-col">
                    <Tabs value={activeTab} onValueChange={setActiveTab} className="flex-1 flex flex-col">
                        {/* Tab Navigation */}
                        <div className="border-b px-4 py-3">
                            <TabsList className="h-10">
                                <TabsTrigger value="dashboard" className="text-xs">
                                    <BarChart3 className="h-4 w-4 mr-1" />
                                    Dashboard
                                </TabsTrigger>
                                <TabsTrigger value="financial" className="text-xs">
                                    <FileText className="h-4 w-4 mr-1" />
                                    Financial Reports
                                    <Badge variant="secondary" className="ml-2 text-xs">
                                        {financialCount}
                                    </Badge>
                                </TabsTrigger>
                                <TabsTrigger value="sales" className="text-xs">
                                    <TrendingUp className="h-4 w-4 mr-1" />
                                    Sales Analytics
                                    <Badge variant="secondary" className="ml-2 text-xs">
                                        {salesCount}
                                    </Badge>
                                </TabsTrigger>
                                <TabsTrigger value="performance" className="text-xs">
                                    <Users className="h-4 w-4 mr-1" />
                                    Employee Performance
                                </TabsTrigger>
                            </TabsList>
                        </div>

                        {/* Tab Content */}
                        <div className="flex-1 overflow-hidden">
                            <TabsContent value="dashboard" className="h-full m-0">
                                <ReportDashboard />
                            </TabsContent>

                            <TabsContent value="financial" className="h-full m-0">
                                <FinancialReports />
                            </TabsContent>

                            <TabsContent value="sales" className="h-full m-0">
                                <SalesAnalytics />
                            </TabsContent>

                            <TabsContent value="performance" className="h-full m-0">
                                <EmployeePerformance />
                            </TabsContent>
                        </div>
                    </Tabs>
                </div>

                {/* Right Sidebar - Quick Actions & Stats */}
                <div className="w-80 border-l bg-muted/20 flex flex-col">
                    {/* Quick Actions */}
                    <div className="border-b p-4">
                        <h3 className="text-sm font-medium mb-3 flex items-center gap-2">
                            <Activity className="h-4 w-4" />
                            Quick Actions
                        </h3>
                        <div className="space-y-2">
                            <Button
                                className="w-full justify-start"
                                size="sm"
                                onClick={handleGenerateReport}
                                disabled={isGenerating}
                            >
                                <Plus className="mr-2 h-4 w-4" />
                                Generate Report
                            </Button>
                            <Button
                                variant="outline"
                                className="w-full justify-start"
                                size="sm"
                                onClick={handleExportReport}
                                disabled={!hasActiveReport || isExporting}
                            >
                                <Download className="mr-2 h-4 w-4" />
                                Export Report
                            </Button>
                        </div>
                    </div>

                    {/* Key Metrics */}
                    <div className="border-b p-4">
                        <h3 className="text-sm font-medium mb-3 flex items-center gap-2">
                            <Target className="h-4 w-4" />
                            Key Metrics
                        </h3>
                        <div className="space-y-3">
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <DollarSign className="h-4 w-4 text-green-500" />
                                    <span className="text-sm">Total Revenue</span>
                                </div>
                                <span className="text-sm font-medium">
                                    ${dashboardMetrics.totalRevenue.toLocaleString()}
                                </span>
                            </div>
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <TrendingUp className="h-4 w-4 text-blue-500" />
                                    <span className="text-sm">Net Profit</span>
                                </div>
                                <span className="text-sm font-medium">
                                    ${dashboardMetrics.totalProfit.toLocaleString()}
                                </span>
                            </div>
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <BarChart3 className="h-4 w-4 text-purple-500" />
                                    <span className="text-sm">Profit Margin</span>
                                </div>
                                <span className="text-sm font-medium">
                                    {dashboardMetrics.profitMargin.toFixed(1)}%
                                </span>
                            </div>
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                    <Users className="h-4 w-4 text-orange-500" />
                                    <span className="text-sm">Total Reports</span>
                                </div>
                                <span className="text-sm font-medium">{totalReports}</span>
                            </div>
                        </div>
                    </div>

                    {/* Recent Activity */}
                    <div className="flex-1 p-4">
                        <h3 className="text-sm font-medium mb-3 flex items-center gap-2">
                            <Calendar className="h-4 w-4" />
                            Recent Activity
                        </h3>
                        <div className="space-y-2">
                            {isGenerating ? (
                                <div className="flex items-center gap-2 text-sm text-muted-foreground">
                                    <div className="w-2 h-2 bg-blue-500 rounded-full animate-pulse" />
                                    Generating report...
                                </div>
                            ) : isExporting ? (
                                <div className="flex items-center gap-2 text-sm text-muted-foreground">
                                    <div className="w-2 h-2 bg-green-500 rounded-full animate-pulse" />
                                    Exporting report...
                                </div>
                            ) : (
                                <div className="text-sm text-muted-foreground">
                                    No recent activity
                                </div>
                            )}
                        </div>
                    </div>

                    {/* Report Status */}
                    <div className="border-t p-4">
                        <h3 className="text-sm font-medium mb-3">Report Status</h3>
                        {hasActiveReport ? (
                            <div className="flex items-center gap-2 text-sm text-green-600">
                                <div className="w-2 h-2 bg-green-500 rounded-full" />
                                Report ready for export
                            </div>
                        ) : (
                            <div className="flex items-center gap-2 text-sm text-muted-foreground">
                                <AlertCircle className="h-4 w-4" />
                                No active report
                            </div>
                        )}
                    </div>
                </div>
            </div>

            {/* Dialogs */}
            <GenerateReportDialog
                isOpen={showGenerateDialog}
                onClose={() => setShowGenerateDialog(false)}
                onSuccess={handleReportGenerated}
            />

            <ExportReportDialog
                isOpen={showExportDialog}
                onClose={() => setShowExportDialog(false)}
                onSuccess={handleReportExported}
            />
        </div>
    );
} 
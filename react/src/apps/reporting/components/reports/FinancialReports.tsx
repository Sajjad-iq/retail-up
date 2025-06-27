import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { FileText, Plus, Download } from 'lucide-react';

/**
 * FinancialReports Component
 * 
 * Financial reports management interface.
 */
export function FinancialReports() {
    return (
        <div className="p-6 space-y-6">
            <div className="flex items-center justify-between">
                <div>
                    <h2 className="text-2xl font-bold">Financial Reports</h2>
                    <p className="text-muted-foreground">
                        Generate and manage comprehensive financial reports including P&L, cash flow, and revenue analysis.
                    </p>
                </div>
                <Button>
                    <Plus className="mr-2 h-4 w-4" />
                    New Report
                </Button>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <FileText className="h-5 w-5" />
                            Monthly P&L Report
                        </CardTitle>
                        <CardDescription>
                            Profit and loss statement for December 2024
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-2">
                            <div className="flex justify-between text-sm">
                                <span>Total Revenue:</span>
                                <span className="font-medium">$125,000</span>
                            </div>
                            <div className="flex justify-between text-sm">
                                <span>Total Expenses:</span>
                                <span className="font-medium">$80,000</span>
                            </div>
                            <div className="flex justify-between text-sm font-medium">
                                <span>Net Profit:</span>
                                <span className="text-green-600">$45,000</span>
                            </div>
                        </div>
                        <Button variant="outline" size="sm" className="w-full mt-4">
                            <Download className="mr-2 h-4 w-4" />
                            Export
                        </Button>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <FileText className="h-5 w-5" />
                            Cash Flow Analysis
                        </CardTitle>
                        <CardDescription>
                            Cash flow statement for Q4 2024
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-2">
                            <div className="flex justify-between text-sm">
                                <span>Operating Cash Flow:</span>
                                <span className="font-medium">$52,000</span>
                            </div>
                            <div className="flex justify-between text-sm">
                                <span>Investing Activities:</span>
                                <span className="font-medium">-$15,000</span>
                            </div>
                            <div className="flex justify-between text-sm font-medium">
                                <span>Free Cash Flow:</span>
                                <span className="text-green-600">$37,000</span>
                            </div>
                        </div>
                        <Button variant="outline" size="sm" className="w-full mt-4">
                            <Download className="mr-2 h-4 w-4" />
                            Export
                        </Button>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <FileText className="h-5 w-5" />
                            Revenue Breakdown
                        </CardTitle>
                        <CardDescription>
                            Revenue analysis by product categories
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-2">
                            <div className="flex justify-between text-sm">
                                <span>Electronics:</span>
                                <span className="font-medium">45%</span>
                            </div>
                            <div className="flex justify-between text-sm">
                                <span>Clothing:</span>
                                <span className="font-medium">28%</span>
                            </div>
                            <div className="flex justify-between text-sm">
                                <span>Home & Garden:</span>
                                <span className="font-medium">27%</span>
                            </div>
                        </div>
                        <Button variant="outline" size="sm" className="w-full mt-4">
                            <Download className="mr-2 h-4 w-4" />
                            Export
                        </Button>
                    </CardContent>
                </Card>
            </div>
        </div>
    );
} 
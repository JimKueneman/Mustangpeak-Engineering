unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  DBGrids, ComCtrls, TAGraph, TASeries;

const
  TaxableIncomeBands = 6;

type
  TTaxBand = array[0..TaxableIncomeBands-1] of extended;
  TTaxableIncomePerBand = array[0..TaxableIncomeBands] of extended;
  TNestEggBins = array of integer;

const
  TaxRate : array[0..TaxableIncomeBands] of extended = (0.10, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37);
  TaxBandMarriedJointly : TTaxBand = (20550, 83550, 178150, 340100, 431900, 647850);

type
  TYearlyDataRec = record
    InterestRate: extended;                        // Random Interest Rate in the market for this year
    NestEggTotal: extended;                        // What is in the NestEgg this year
    OutsideIncome: extended;                       // What income did we have this year
    TaxesOnPreviousYearsIncome: extended;          // How much did we end up paying in taxes last year
    Expendatures: extended;                        // How much went out this year
    PulledFromNestEgg: extended;                   // How much did we have to use from the NestEgg
    Ideal4PercentOfNestEgg: extended;              // Straight calculation of 4% of the NestEgg
    NetOffsetFromIdeal4PercentOfNestEgg: extended; // What is the delta from the ideal 4% max we should be using from the NestEgg, negative is over 4% positive is less than 4%
    PreviousYearTaxableIncome: extended            // What was the total Taxable income from last year.
  end;

  TLifetimeDataArray = array of TYearlyDataRec;
  TStatisticalYearlyArray = array of TLifetimeDataArray;

  { TForm1 }

  TForm1 = class(TForm)
    BarSeriesStatistics: TBarSeries;
    BarSeriesStatisticsUserYear: TBarSeries;
    Button1: TButton;
    ChartNetGains: TChart;
    ChartIdeal4Percent: TChart;
    ChartOffsetFromIdeal4Percent: TChart;
    ChartInterest: TChart;
    ChartExpendatures: TChart;
    ChartStatistics: TChart;
    ChartStatisticsUserYear: TChart;
    ChartTaxes: TChart;
    ChartNestEgg: TChart;
    ChartIncome: TChart;
    ChartYearlyGross: TChart;
    ChartPreviousYearTaxableIncome: TChart;
    CheckBoxIncludeBadYear: TCheckBox;
    CheckBoxInflation: TCheckBox;
    CheckBoxMultiRun: TCheckBox;
    CheckBoxTaxFreeSource: TCheckBox;
    FloatSpinEditAvgTargetMax: TFloatSpinEdit;
    FloatSpinEditAvgTargetMin: TFloatSpinEdit;
    FloatSpinEditInflation: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label7: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelInterestAverage: TLabel;
    LabelInterestConsectutiveYearLoss: TLabel;
    LabelInterestMaxGain: TLabel;
    LabelInterestMaxLoss: TLabel;
    LabelInterestPercentLoss: TLabel;
    LabelNestEggEnd: TLabel;
    LabelUtilities: TLabel;
    LineSeriesNetGains: TLineSeries;
    LineSeriesIdeal4Percent: TLineSeries;
    LineSeriesOffsetFromIdeal4Percent: TLineSeries;
    LineSeriesInterest: TLineSeries;
    LineSeriesExpendatures: TLineSeries;
    LineSeriesTaxes: TLineSeries;
    LineSeriesNestEgg: TLineSeries;
    LineSeriesIncome: TLineSeries;
    LineSeriesYearlyGross: TLineSeries;
    LineSeriesPreviousYearTaxableIncome: TLineSeries;
    PageControl1: TPageControl;
    SpinEditCarInsurance: TSpinEdit;
    SpinEditCarInsuranceEnd: TSpinEdit;
    SpinEditCarInsuranceStart: TSpinEdit;
    SpinEditCarLoan: TSpinEdit;
    SpinEditCarLoanEnd: TSpinEdit;
    SpinEditCarLoanStart: TSpinEdit;
    SpinEditCherylAge: TSpinEdit;
    SpinEditCherylMaxAge: TSpinEdit;
    SpinEditCherylSocialSecurity: TSpinEdit;
    SpinEditCollegeTuition: TSpinEdit;
    SpinEditCollegeLivingExpenses: TSpinEdit;
    SpinEditCollegeTuitionEnd: TSpinEdit;
    SpinEditCollegeLivingExpensesEnd: TSpinEdit;
    SpinEditCollegeTuitionStart: TSpinEdit;
    SpinEditCollegeLivingExpensesStart: TSpinEdit;
    SpinEditConsecutiveLossYearsTarget: TSpinEdit;
    SpinEditGroceries: TSpinEdit;
    SpinEditGroceriesEnd: TSpinEdit;
    SpinEditGroceriesStart: TSpinEdit;
    SpinEditHealthInsurance: TSpinEdit;
    SpinEditHealthInsuranceChildren: TSpinEdit;
    SpinEditHealthInsuranceChildrenEnd: TSpinEdit;
    SpinEditHealthInsuranceChildrenStart: TSpinEdit;
    SpinEditHealthInsuranceEnd: TSpinEdit;
    SpinEditHealthInsuranceStart: TSpinEdit;
    SpinEditJimAge: TSpinEdit;
    SpinEditJimMaxAge: TSpinEdit;
    SpinEditJimPension: TSpinEdit;
    SpinEditJimPensionYears: TSpinEdit;
    SpinEditJimSocialSecurity: TSpinEdit;
    SpinEditLongTermCare: TSpinEdit;
    SpinEditLongTermCareEnd: TSpinEdit;
    SpinEditLongTermCareStart: TSpinEdit;
    SpinEditMortgage: TSpinEdit;
    SpinEditMortgageEnd: TSpinEdit;
    SpinEditMortgageStart: TSpinEdit;
    SpinEditMultiRun: TSpinEdit;
    SpinEditMultiRunUserYearNestEgg: TSpinEdit;
    SpinEditNestEgg: TSpinEdit;
    SpinEditOutsideIncome1: TSpinEdit;
    SpinEditOutsideIncome2: TSpinEdit;
    SpinEditOutsideIncomeEnd1: TSpinEdit;
    SpinEditOutsideIncomeEnd2: TSpinEdit;
    SpinEditOutsideIncomeStart1: TSpinEdit;
    SpinEditOutsideIncomeStart2: TSpinEdit;
    SpinEditPhone: TSpinEdit;
    SpinEditPhoneEnd: TSpinEdit;
    SpinEditPhoneStart: TSpinEdit;
    SpinEditPropertyTaxes: TSpinEdit;
    SpinEditPropertyTaxesEnd: TSpinEdit;
    SpinEditPropertyTaxesStart: TSpinEdit;
    SpinEditRetirementAge: TSpinEdit;
    SpinEditUtilities: TSpinEdit;
    SpinEditUtilitiesEnd: TSpinEdit;
    SpinEditUtilitiesStart: TSpinEdit;
    SpinEditYearlyGrossSalary: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure SpinEditJimPensionChange(Sender: TObject);
    procedure SpinEditRetirementAgeChange(Sender: TObject);
  private
    FStatisticalYearlyArray: TStatisticalYearlyArray;
    FYearlyDataArray: TLifetimeDataArray;

  public
    property YearlyDataArray: TLifetimeDataArray read FYearlyDataArray write FYearlyDataArray;
    property StatisticalYearlyArray: TStatisticalYearlyArray read FStatisticalYearlyArray write FStatisticalYearlyArray;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  i, j, CountLoss, CountGain, ConsecutiveLossYears, ConsecutiveLossYearCounter, Looper, LooperMax,
    MaxLossIndex, MaxGainIndex, YearsToEnd, YearsToRetirement, YearsInRetirement, OutOfMoneyCount: Integer;
  Sum, Avg, PercentLoss, MaxLoss, MaxGain: Extended;
  NestEggEnd, MinNestEgg, MinNestEggAt15Years, AvgNestEgg, AvgNestEggAt15Years: Currency;
  TaxBand: TTaxBand;
  TaxableIncomePerBand: TTaxableIncomePerBand;
  MinNextEggBin, FifteenYearNestEggBin: TNestEggBins;
  OutOfMoney: Boolean;
begin

  MinNestEgg := 1e10;
  MinNestEggAt15Years := 1e10;
  AvgNestEgg := 0;
  AvgNestEggAt15Years := 0;
  OutOfMoneyCount := 0;

  SetLength(MinNextEggBin, 5);  // 0 = <500k, 1 = 500k-1M, 2 = 1M-5M, 3 = 5M-10M, 4 = >10M
  MinNextEggBin[0] := 0;
  MinNextEggBin[1] := 0;
  MinNextEggBin[2] := 0;
  MinNextEggBin[3] := 0;
  MinNextEggBin[4] := 0;

  SetLength(FifteenYearNestEggBin, 5);  // 0 = <500k, 1 = 500k-1M, 2 = 1M-5M, 3 = 5M-10M, 4 = >10M
  FifteenYearNestEggBin[0] := 0;
  FifteenYearNestEggBin[1] := 0;
  FifteenYearNestEggBin[2] := 0;
  FifteenYearNestEggBin[3] := 0;
  FifteenYearNestEggBin[4] := 0;

  TaxBand := TaxBandMarriedJointly;

  YearsToEnd := SpinEditJimMaxAge.Value - SpinEditJimAge.Value;
  YearsToRetirement := SpinEditRetirementAge.Value - SpinEditJimAge.Value;
  YearsInRetirement := YearsToEnd - YearsToRetirement;

  SetLength(FYearlyDataArray, YearsInRetirement);

  if CheckBoxMultiRun.Checked then
    LooperMax := SpinEditMultiRun.Value
  else
    LooperMax := 0;


  SetLength(FStatisticalYearlyArray, LooperMax+1);

  for Looper := 0 to LooperMax do
  begin

    // Calculate a random set of Monthly Inerest Rate Gains/Losses that meet a criteria that is similiar to historical stock market
    // and populate them in the Monthly Data Array
    repeat
      Avg := 0;
      CountGain := 0;
      CountLoss := 0;
      ConsecutiveLossYearCounter := 0;
      ConsecutiveLossYears := 0;
      MaxLoss := 0;
      MaxGain := 0;
      OutOfMoney := False;

      // Calculate the random interest rates per month for the life term based on criteria
      for i := 0 to Length(YearlyDataArray)-1 do
      begin
        FYearlyDataArray[i].InterestRate := Random(50)/100 - 0.17;
        if FYearlyDataArray[i].InterestRate < 0 then
          Inc(CountLoss)
        else
          Inc(CountGain);

        if FYearlyDataArray[i].InterestRate >= 0 then
          ConsecutiveLossYearCounter := 0
        else
          Inc(ConsecutiveLossYearCounter);

        if ConsecutiveLossYearCounter > ConsecutiveLossYears then
          ConsecutiveLossYears := ConsecutiveLossYearCounter;

        // Search for the highest return in the life span
        if FYearlyDataArray[i].InterestRate > MaxGain then
        begin
          MaxGain := FYearlyDataArray[i].InterestRate;
          MaxGainIndex := i
        end;
        // Search for the highest loss in the life span
        if FYearlyDataArray[i].InterestRate < MaxLoss then
        begin
          MaxLoss := FYearlyDataArray[i].InterestRate;
          MaxLossIndex := i
        end;
      end;


      // Calculate the average rate of return
      Sum := 0;
      for i := 0 to Length(YearlyDataArray)-1 do
        Sum := Sum + FYearlyDataArray[i].InterestRate;
      Avg := Sum/Length(YearlyDataArray);

      // Calculate the percentage of months that were a loss
      PercentLoss := CountLoss/(CountLoss+CountGain);

    until (Avg >= FloatSpinEditAvgTargetMin.Value/100) and      // Average is in the window defined
          (Avg <= FloatSpinEditAvgTargetMax.Value/100) and
          (PercentLoss < 0.40) and                              // Less than 40% of the years were losses
          (ConsecutiveLossYears <= SpinEditConsecutiveLossYearsTarget.Value); // No more than the defined consecutive loss years are found


    // Make one years losses as bad as the best good year; this will change the average but that is fine
    if CheckBoxIncludeBadYear.Checked then
    begin
      FYearlyDataArray[MaxLossIndex].InterestRate := -YearlyDataArray[MaxGainIndex].InterestRate;  // Make one really bad year equal to the best year.
      MaxLoss := FYearlyDataArray[MaxLossIndex].InterestRate;
    end;

    // Calculate Interest Statistics
    LabelInterestAverage.Caption := 'Interest Average:  ' + FloatToStrF(Avg * 100, ffGeneral, 2, 2) + '%' ;
    LabelInterestPercentLoss.Caption := 'Interest Percent at Loss:  ' + FloatToStrF(PercentLoss * 100, ffGeneral, 2, 2) + '%' ;
    LabelInterestConsectutiveYearLoss.Caption := 'Interest Consecutive Loss Years Max: ' + IntToStr(ConsecutiveLossYears) + ' Years' ;
    LabelInterestMaxGain.Caption := 'Interest Max Gain:  ' + FloatToStrF(MaxGain * 100, ffGeneral, 2, 2) + '%' ;
    LabelInterestMaxLoss.Caption := 'Interest Max Loss:  ' + FloatToStrF(MaxLoss * 100, ffGeneral, 2, 2) + '%' ;

    //--------------------------------------------------------------------------
    // Now start calculating the information based on each months random interest rate
    //--------------------------------------------------------------------------

    NestEggEnd := 0;
    for i := 0 to Length(YearlyDataArray)-1 do
    begin
      // Calculate Compound Interest for each year
      if i = 0 then
        FYearlyDataArray[i].NestEggTotal := SpinEditNestEgg.Value
      else
        FYearlyDataArray[i].NestEggTotal := FYearlyDataArray[i-1].NestEggTotal + (FYearlyDataArray[i-1].NestEggTotal*FYearlyDataArray[i].InterestRate);

      if not OutOfMoney then
      begin
        OutOfMoney := FYearlyDataArray[i].NestEggTotal <= 0;
        if OutOfMoney then
          Inc(OutOfMoneyCount);
      end;

      //------------------------------------------------------------------------
      // Calculate the income outside of investments for the month
      //------------------------------------------------------------------------
      FYearlyDataArray[i].OutsideIncome := 0;
         // Still getting the pension?
      if i < SpinEditJimPensionYears.Value then
        FYearlyDataArray[i].OutsideIncome := FYearlyDataArray[i].OutsideIncome + (SpinEditJimPension.Value*12);
         // Old enough for Jim's Social Security?
      if (i + SpinEditRetirementAge.Value >= 65) and (i + SpinEditRetirementAge.Value <= SpinEditJimMaxAge.Value) then
        FYearlyDataArray[i].OutsideIncome := FYearlyDataArray[i].OutsideIncome + (SpinEditJimSocialSecurity.Value);
        // Old enough for Cheryl's Social Security?
      if (i + SpinEditRetirementAge.Value - (SpinEditJimAge.Value - SpinEditCherylAge.Value) >= 65) and (i + SpinEditRetirementAge.Value - (SpinEditJimAge.Value - SpinEditCherylAge.Value) <= SpinEditCherylMaxAge.Value) then
        FYearlyDataArray[i].OutsideIncome := FYearlyDataArray[i].OutsideIncome + (SpinEditCherylSocialSecurity.Value);

      if (i >= SpinEditOutsideIncomeStart1.Value) and (i <= SpinEditOutsideIncomeEnd1.Value) then
        FYearlyDataArray[i].OutsideIncome := FYearlyDataArray[i].OutsideIncome + SpinEditOutsideIncome1.Value;
      if (i >= SpinEditOutsideIncomeStart2.Value) and (i <= SpinEditOutsideIncomeEnd2.Value) then
        FYearlyDataArray[i].OutsideIncome := FYearlyDataArray[i].OutsideIncome + SpinEditOutsideIncome2.Value;

      //------------------------------------------------------------------------------

      //------------------------------------------------------------------------------
      // Calculate Taxes on the Income for previous year
      //------------------------------------------------------------------------------
      if i > 0 then
      begin
        FYearlyDataArray[i].PreviousYearTaxableIncome := FYearlyDataArray[i-1].OutsideIncome;

        if not CheckBoxTaxFreeSource.Checked then
          // if there was not enough income to cover the expendatures + the taxes paid that year then we must have pulled money from the investments to cover it
          if FYearlyDataArray[i-1].NetOffsetFromIdeal4PercentOfNestEgg < 0 then // Did pull money from investments so we need to add that to the taxable income from last year and need to pay taxes on it (Roth is an exception but this is worst case)
             YearlyDataArray[i].PreviousYearTaxableIncome := YearlyDataArray[i].PreviousYearTaxableIncome + abs(FYearlyDataArray[i-1].NetOffsetFromIdeal4PercentOfNestEgg);

        // Calculate taxes to pay in this month based on previous years income using the sliding tax charts
        TaxableIncomePerBand[0] := 0;
        TaxableIncomePerBand[1] := 0;
        TaxableIncomePerBand[2] := 0;
        TaxableIncomePerBand[3] := 0;
        TaxableIncomePerBand[4] := 0;
        TaxableIncomePerBand[5] := 0;
        TaxableIncomePerBand[6] := 0;

        if YearlyDataArray[i].PreviousYearTaxableIncome <= TaxBand[0] then
        begin
          TaxableIncomePerBand[0] := YearlyDataArray[i].PreviousYearTaxableIncome
        end else
        if FYearlyDataArray[i].OutsideIncome <= TaxBand[1] then
        begin
          TaxableIncomePerBand[0] := TaxBand[0];
          TaxableIncomePerBand[1] := YearlyDataArray[i].PreviousYearTaxableIncome - TaxBand[0]
        end else
        if YearlyDataArray[i].PreviousYearTaxableIncome <= TaxBand[2] then
        begin
          TaxableIncomePerBand[0] := TaxBand[0];
          TaxableIncomePerBand[1] := TaxBand[1];
          TaxableIncomePerBand[2] := YearlyDataArray[i].PreviousYearTaxableIncome - TaxBand[1]
        end else
        if YearlyDataArray[i].PreviousYearTaxableIncome <= TaxBand[3] then
        begin
          TaxableIncomePerBand[0] := TaxBand[0];
          TaxableIncomePerBand[1] := TaxBand[1];
          TaxableIncomePerBand[2] := TaxBand[2];
          TaxableIncomePerBand[3] := YearlyDataArray[i].PreviousYearTaxableIncome - TaxBand[2]
        end else
        if YearlyDataArray[i].PreviousYearTaxableIncome <= TaxBand[4] then
        begin
          TaxableIncomePerBand[0] := TaxBand[0];
          TaxableIncomePerBand[1] := TaxBand[1];
          TaxableIncomePerBand[2] := TaxBand[2];
          TaxableIncomePerBand[3] := TaxBand[3];
          TaxableIncomePerBand[4] := YearlyDataArray[i].PreviousYearTaxableIncome - TaxBand[3]
        end else
        if YearlyDataArray[i].PreviousYearTaxableIncome <= TaxBand[5] then
        begin
          TaxableIncomePerBand[0] := TaxBand[0];
          TaxableIncomePerBand[1] := TaxBand[1];
          TaxableIncomePerBand[2] := TaxBand[2];
          TaxableIncomePerBand[3] := TaxBand[3];
          TaxableIncomePerBand[4] := TaxBand[4];
          TaxableIncomePerBand[5] := YearlyDataArray[i].PreviousYearTaxableIncome - TaxBand[3]
        end else
        begin
          TaxableIncomePerBand[0] := TaxBand[0];
          TaxableIncomePerBand[1] := TaxBand[1];
          TaxableIncomePerBand[2] := TaxBand[2];
          TaxableIncomePerBand[3] := TaxBand[3];
          TaxableIncomePerBand[4] := TaxBand[4];
          TaxableIncomePerBand[5] := TaxBand[5];
          TaxableIncomePerBand[6] := YearlyDataArray[i].PreviousYearTaxableIncome - TaxBand[5]
        end;

        FYearlyDataArray[i].TaxesOnPreviousYearsIncome := 0;
        for j := 0 to TaxableIncomeBands - 1 do
          FYearlyDataArray[i].TaxesOnPreviousYearsIncome := FYearlyDataArray[i].TaxesOnPreviousYearsIncome + (TaxRate[j] * TaxableIncomePerBand[j]);

      end else
        FYearlyDataArray[i].TaxesOnPreviousYearsIncome := 0;
      //------------------------------------------------------------------------------


      //------------------------------------------------------------------------------
      // Calculate Total Expendatures
      FYearlyDataArray[i].Expendatures := SpinEditYearlyGrossSalary.Value;

      if (i >= SpinEditMortgageStart.Value) and (i <= SpinEditMortgageEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures + SpinEditMortgage.Value*12;

      if (i >= SpinEditHealthInsuranceStart.Value) and (i <= SpinEditHealthInsuranceEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures + SpinEditHealthInsurance.Value*12;

      if (i >= SpinEditUtilitiesStart.Value) and (i <= SpinEditUtilitiesEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditUtilities.Value*12;

      if (i >= SpinEditPhoneStart.Value) and (i <= SpinEditPhoneEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditPhone.Value*12;

      if (i >= SpinEditCarLoanStart.Value) and (i <= SpinEditCarLoanEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditCarLoan.Value*12;

      if (i >= SpinEditPropertyTaxesStart.Value) and (i <= SpinEditPropertyTaxesEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditPropertyTaxes.Value*12;

      if (i >= SpinEditCarInsuranceStart.Value) and (i <= SpinEditCarInsuranceEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditCarInsurance.Value*12;

      if (i >= SpinEditGroceriesStart.Value) and (i <= SpinEditGroceriesEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditGroceries.Value*12;

      if (i >= SpinEditLongTermCareStart.Value) and (i <= SpinEditLongTermCareEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditLongTermCare.Value*12;

      if (i >= SpinEditCollegeTuitionStart.Value) and (i <= SpinEditCollegeTuitionEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditCollegeTuition.Value*12;

      if (i >= SpinEditHealthInsuranceChildrenStart.Value) and (i <= SpinEditHealthInsuranceChildrenEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditHealthInsuranceChildren.Value*12;

      if (i >= SpinEditCollegeLivingExpensesStart.Value) and (i <= SpinEditCollegeLivingExpensesEnd.Value) then
        FYearlyDataArray[i].Expendatures := FYearlyDataArray[i].Expendatures +  SpinEditCollegeLivingExpenses.Value*12;

      //------------------------------------------------------------------------------

      //------------------------------------------------------------------------------
      // Add in inflation to the Expendatures
      if CheckBoxInflation.Checked then
      begin
        if i > 0 then
        begin
          for j := 0 to i - 1 do
           FYearlyDataArray[i].Expendatures := FYearlyDataArray[i-1].Expendatures * (1+(FloatSpinEditInflation.Value/100))
        end
      end;
      //-----------------------------------------------------------------------------


      // NetGains for the year are all the Outside Income minus living expenses and what we had to pay in taxes; Minus=pulled from investments; Plus=added to investments
      FYearlyDataArray[i].PulledFromNestEgg := FYearlyDataArray[i].OutsideIncome - (FYearlyDataArray[i].Expendatures + FYearlyDataArray[i].TaxesOnPreviousYearsIncome);
      // What is the 4% rule max would should have pulled from the nest egg?
      FYearlyDataArray[i].Ideal4PercentOfNestEgg := FYearlyDataArray[i].NestEggTotal * 0.04;
      // If DeltaInNestEgg is negative more than the ideal 4% this will be negative
      FYearlyDataArray[i].NetOffsetFromIdeal4PercentOfNestEgg := FYearlyDataArray[i].Ideal4PercentOfNestEgg + FYearlyDataArray[i].PulledFromNestEgg;
      // Update the NestEgg depending on what we pulled or put back.
      FYearlyDataArray[i].NestEggTotal := FYearlyDataArray[i].NestEggTotal + FYearlyDataArray[i].PulledFromNestEgg;

      // Save this scenario for later analysis
      FStatisticalYearlyArray[Looper] := FYearlyDataArray;


      // Find some simple statistics, the mininum value of the nestegg
      if FYearlyDataArray[i].NestEggTotal < MinNestEgg then
        MinNestEgg := FYearlyDataArray[i].NestEggTotal;

      // Build up the sum for the final average; note for multiple scenarios this sums ALL years across all scenerios
      AvgNestEgg := AvgNestEgg + FYearlyDataArray[i].NestEggTotal;

      // Find minimum and average at the 15 year point: note for multiple scenarios this is across ALL years and across all scenerios
      if i = 15 then
      begin
        if FYearlyDataArray[i].NestEggTotal < MinNestEggAt15Years then
          MinNestEggAt15Years := FYearlyDataArray[i].NestEggTotal;

        AvgNestEggAt15Years := AvgNestEggAt15Years + FYearlyDataArray[i].NestEggTotal;
      end;

      // Bin
      if FYearlyDataArray[i].NestEggTotal < 0.5e6 then
        MinNextEggBin[0] := MinNextEggBin[0] + 1
      else
        if FYearlyDataArray[i].NestEggTotal < 1e6 then
          MinNextEggBin[1] := MinNextEggBin[1] + 1
      else
        if FYearlyDataArray[i].NestEggTotal < 5e6 then
          MinNextEggBin[2] := MinNextEggBin[2] + 1
      else
        if FYearlyDataArray[i].NestEggTotal < 10e6 then
          MinNextEggBin[3] := MinNextEggBin[3] + 1
      else
          MinNextEggBin[4] := MinNextEggBin[4] + 1;

      if i = SpinEditMultiRunUserYearNestEgg.Value-1 then
      begin
        if FYearlyDataArray[i].NestEggTotal < 0.5e6 then
          FifteenYearNestEggBin[0] := FifteenYearNestEggBin[0] + 1
        else
          if FYearlyDataArray[i].NestEggTotal < 1e6 then
            FifteenYearNestEggBin[1] := FifteenYearNestEggBin[1] + 1
        else
          if FYearlyDataArray[i].NestEggTotal < 5e6 then
            FifteenYearNestEggBin[2] := FifteenYearNestEggBin[2] + 1
        else
          if FYearlyDataArray[i].NestEggTotal < 10e6 then
            FifteenYearNestEggBin[3] := FifteenYearNestEggBin[3] + 1
        else
            FifteenYearNestEggBin[4] := FifteenYearNestEggBin[4] + 1;
      end;

    end;  // Done looping through every year for one scenario
  end; // Done looping through all the scenairos

  NestEggEnd := NestEggEnd + FYearlyDataArray[Length(FYearlyDataArray)-1].NestEggTotal;

  LabelNestEggEnd.Caption := FloatToStrF(NestEggEnd, ffCurrency, 2, 0);

  LineSeriesInterest.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesInterest.AddXY(i, YearlyDataArray[i].InterestRate*100);

  LineSeriesNestEgg.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesNestEgg.AddXY(i, YearlyDataArray[i].NestEggTotal);

  LineSeriesIncome.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesIncome.AddXY(i, YearlyDataArray[i].OutsideIncome);

  LineSeriesExpendatures.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesExpendatures.AddXY(i, YearlyDataArray[i].Expendatures);

  LineSeriesTaxes.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesTaxes.AddXY(i, YearlyDataArray[i].TaxesOnPreviousYearsIncome);

  LineSeriesNetGains.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesNetGains.AddXY(i, YearlyDataArray[i].PulledFromNestEgg);

  LineSeriesIdeal4Percent.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesIdeal4Percent.AddXY(i, YearlyDataArray[i].Ideal4PercentOfNestEgg);

  LineSeriesOffsetFromIdeal4Percent.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesOffsetFromIdeal4Percent.AddXY(i, YearlyDataArray[i].NetOffsetFromIdeal4PercentOfNestEgg);

  LineSeriesYearlyGross.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesYearlyGross.AddXY(i, FYearlyDataArray[i].Expendatures + FYearlyDataArray[i].TaxesOnPreviousYearsIncome);

  LineSeriesPreviousYearTaxableIncome.Clear;
  for i := 0 to Length(YearlyDataArray)-1 do
    LineSeriesPreviousYearTaxableIncome.AddXY(i, FYearlyDataArray[i].PreviousYearTaxableIncome);


  BarSeriesStatistics.Clear;
  BarSeriesStatistics.AddXY(0, MinNextEggBin[0], '<$500k', clRed);
  BarSeriesStatistics.AddXY(1, MinNextEggBin[1], '$500k-$1M', clPurple);
  BarSeriesStatistics.AddXY(2, MinNextEggBin[2], '$1M-$5M', clYellow);
  BarSeriesStatistics.AddXY(3, MinNextEggBin[3], '$5M-$10M', clLime);
  BarSeriesStatistics.AddXY(4, MinNextEggBin[4], '>$10M', clGreen);

  BarSeriesStatisticsUserYear.Clear;
  BarSeriesStatisticsUserYear.AddXY(0, FifteenYearNestEggBin[0], '<$500k', clRed);
  BarSeriesStatisticsUserYear.AddXY(1, FifteenYearNestEggBin[1], '$500k-$1M', clPurple);
  BarSeriesStatisticsUserYear.AddXY(2, FifteenYearNestEggBin[2], '$1M-$5M', clYellow);
  BarSeriesStatisticsUserYear.AddXY(3, FifteenYearNestEggBin[3], '$5M-$10M', clLime);
  BarSeriesStatisticsUserYear.AddXY(4, FifteenYearNestEggBin[4], '>$10M', clGreen);

  if CheckBoxMultiRun.Checked then
  begin
    ShowMessage('Min NestEgg: ' + FloatToStrF(MinNestEgg, ffCurrency, 2, 0) + #10+#13 +
                'Min NestEgg at 15 years: ' + FloatToStrF(MinNestEggAt15Years, ffCurrency, 2, 0) + #10+#13 +
                'Avg Nest Egg: ' + FloatToStrF(AvgNestEgg/((LooperMax+1)*Length(YearlyDataArray)), ffCurrency, 2, 0) + #10+#13 +
                'Avg NestEgg at 15 years: ' + FloatToStrF(AvgNestEggAt15Years/LooperMax+1, ffCurrency, 2, 0) + #10+#13 +
                'Out of money count: ' + IntToStr(OutOfMoneyCount)
                );
  end;

  Form1.Invalidate;
  Form1.Update;
end;

procedure TForm1.Label3Click(Sender: TObject);
begin

end;

procedure TForm1.SpinEditJimPensionChange(Sender: TObject);
begin

end;

procedure TForm1.SpinEditRetirementAgeChange(Sender: TObject);
begin

end;

end.


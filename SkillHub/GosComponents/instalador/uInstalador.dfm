object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  ActiveControl = edtLogin
  BorderIcons = [biSystemMenu, biMinimize]
  ClientHeight = 459
  ClientWidth = 818
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Wizard: TJvWizard
    Left = 0
    Top = 0
    Width = 818
    Height = 459
    ActivePage = wzPageLogin
    ButtonBarHeight = 0
    ButtonStart.Caption = 'To &Start Page'
    ButtonStart.NumGlyphs = 1
    ButtonStart.Width = 85
    ButtonLast.Caption = 'To &Last Page'
    ButtonLast.NumGlyphs = 1
    ButtonLast.Width = 85
    ButtonBack.Caption = '< &Back'
    ButtonBack.NumGlyphs = 1
    ButtonBack.Width = 75
    ButtonNext.Caption = '&Next >'
    ButtonNext.NumGlyphs = 1
    ButtonNext.Width = 75
    ButtonFinish.Caption = '&Finish'
    ButtonFinish.NumGlyphs = 1
    ButtonFinish.Width = 75
    ButtonCancel.Caption = 'Cancel'
    ButtonCancel.NumGlyphs = 1
    ButtonCancel.ModalResult = 2
    ButtonCancel.Width = 75
    ButtonHelp.Caption = '&Help'
    ButtonHelp.NumGlyphs = 1
    ButtonHelp.Width = 75
    ShowRouteMap = True
    OnFinishButtonClick = WizardFinishButtonClick
    OnCancelButtonClick = WizardCancelButtonClick
    DesignSize = (
      818
      459)
    object wzPageLogin: TJvWizardWelcomePage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Visible = False
      Header.Title.Text = 'Connect'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = False
      Header.Subtitle.Text = 'Subtitle'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      Header.ShowDivider = False
      EnabledButtons = []
      VisibleButtons = []
      WaterMark.Visible = False
      object Label1: TLabel
        Left = 108
        Top = 93
        Width = 144
        Height = 13
        Cursor = crHandPoint
        Caption = 'E-mail (the same of site store)'
        OnClick = Label1Click
      end
      object Label2: TLabel
        Left = 108
        Top = 149
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object lblStatus: TLabel
        Left = 108
        Top = 280
        Width = 433
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'lblStatus'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Visible = False
      end
      object edtLogin: TEdit
        Left = 108
        Top = 112
        Width = 433
        Height = 21
        TabOrder = 0
        TextHint = 'E-mail'
      end
      object edtPassword: TEdit
        Left = 108
        Top = 168
        Width = 433
        Height = 21
        PasswordChar = '*'
        TabOrder = 1
        TextHint = 'Password'
      end
      object btnConnect: TButton
        Left = 428
        Top = 232
        Width = 113
        Height = 25
        Caption = 'Connect'
        TabOrder = 2
        OnClick = btnConnectClick
      end
      object chkRemember: TCheckBox
        Left = 460
        Top = 201
        Width = 81
        Height = 17
        BiDiMode = bdRightToLeft
        Caption = 'Remember  '
        ParentBiDiMode = False
        TabOrder = 3
      end
    end
    object wzPageWelcome: TJvWizardInteriorPage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'Welcome'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = False
      Header.Subtitle.Text = 'Subtitle'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      VisibleButtons = [bkNext, bkCancel]
      Color = clWhite
      OnNextButtonClick = wzPageWelcomeNextButtonClick
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbl1: TLabel
        Left = 24
        Top = 24
        Width = 506
        Height = 16
        Caption = 
          'This installer will assist in the installation of the advanced c' +
          'omponents by  Unix Software'
        Color = 11103753
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Visible = False
      end
      object lbl2: TLabel
        Left = 24
        Top = 80
        Width = 134
        Height = 16
        Caption = 'For support contact:'
        Color = clGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lbl3: TLabel
        Left = 24
        Top = 102
        Width = 142
        Height = 16
        Cursor = crIBeam
        Caption = 'gabriel.o.s@hotmail.com'
        Color = 11103753
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lbl4: TLabel
        Left = 24
        Top = 144
        Width = 57
        Height = 16
        Caption = 'Linkedin:'
        Color = clGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lbl5: TLabel
        Left = 24
        Top = 166
        Width = 287
        Height = 16
        Cursor = crHandPoint
        Caption = 'https://www.linkedin.com/in/gabrieloliveiradelphi/'
        Color = 11103753
        DragCursor = crHandPoint
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = lblSiteClick
      end
      object lbl6: TLabel
        Left = 24
        Top = 212
        Width = 373
        Height = 16
        Caption = 'To stay up to date or purchase new components, access:'
        Color = clGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Visible = False
      end
      object lbl7: TLabel
        Left = 24
        Top = 234
        Width = 204
        Height = 16
        Cursor = crHandPoint
        Caption = 'https://store.falconsistemas.com.br'
        Color = 11103753
        DragCursor = crHandPoint
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        Visible = False
        OnClick = lblSiteClick
      end
    end
    object wzPageConfiguration: TJvWizardInteriorPage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'Configuration'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Subtitle'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      Color = clWhite
      OnNextButtonClick = wzPageConfigurationNextButtonClick
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        641
        394)
      object lbl10: TLabel
        Left = 24
        Top = 14
        Width = 85
        Height = 13
        Caption = 'Versions of Delphi'
        Color = 11103753
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lbl11: TLabel
        Left = 240
        Top = 110
        Width = 46
        Height = 13
        Caption = 'Plataform'
        Color = 11103753
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Visible = False
      end
      object lbl12: TLabel
        Left = 256
        Top = 179
        Width = 331
        Height = 11
        Caption = 
          'Recommended for those who already have the components of Falcon ' +
          'Sistemas.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object chkDelphiVersion: TCheckListBox
        Left = 24
        Top = 33
        Width = 201
        Height = 311
        ItemHeight = 13
        TabOrder = 0
      end
      object cmbPlatform: TComboBox
        Left = 240
        Top = 124
        Width = 313
        Height = 21
        ItemIndex = 0
        TabOrder = 1
        Text = 'Win32'
        Visible = False
        Items.Strings = (
          'Win32'
          'Win64')
      end
      object cmbDelphiVersion: TComboBox
        Left = 240
        Top = 207
        Width = 52
        Height = 21
        TabOrder = 2
        Visible = False
        OnChange = cmbDelphiVersionChange
      end
      object ckbRemoverArquivosAntigos: TCheckBox
        Left = 240
        Top = 158
        Width = 313
        Height = 22
        Caption = 'Remove old files from disk;'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object grpRegisterUnigui: TGroupBox
        Left = 240
        Top = 248
        Width = 390
        Height = 96
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Register of uniGUI'
        TabOrder = 4
        Visible = False
        object lbluniGUI: TLabel
          Left = 16
          Top = 28
          Width = 69
          Height = 13
          Caption = 'FMSoft uniGUI'
        end
        object lblUniGuiWeb: TLabel
          Left = 16
          Top = 49
          Width = 57
          Height = 13
          Caption = 'uniGUI Web'
        end
        object lblUniGuiMobile: TLabel
          Left = 16
          Top = 69
          Width = 65
          Height = 13
          Caption = 'uniGUI Mobile'
        end
        object imgUniGuiWebYes: TImage
          Left = 85
          Top = 47
          Width = 16
          Height = 16
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
            001008060000001FF3FF61000000097048597300000EC400000EC401952B0E1B
            000001C54944415478DA8D934D2883711CC77FCF339E36EFCAD21C48C2453908
            879D0C279C266E23070707A51C0CAD4C58D9C65971D0949D8C7693CC4B997263
            E580BC84421407129BEFEF99FF3CAD95FDEAD3FFEDF7FDFEDFA5582C46DA68F7
            3B6B50F48366500638E1126C828560B7E3549B2F09030815141E3000644A1DDF
            601E8CC0E82B61F02B0E82564A2F3680954D8481171D43698A454CC1605C6A5B
            9DA846230274FF298A0C7954A0CFA1B3973B6E7E822A36984565F83FB111E269
            4B0FE52A7A1A0BF9E8FCE59EBB27D9E008953A91284B1245936EC668C887D846
            A69C428C45C97D10A0DD9B131EDA6783475E1DB71A4C95D45BDB428E9D157A7A
            7F4D885D101743FC1D8DD2DCE13A85AE8F85F7351B3C705E9E92458B1D8364C8
            50E8EEED99ECDBCBEA6A5C4D7F626F38403BF199455CB1C1214FCEADC6926AB2
            9B3B2943D6A9263A4986B840157BC289656B23C40653A88C8A9E865F934C397E
            2971F11AC4915467AB5E63392AFC3C958409CEC26EEE52B7E086782FB5F81D54
            888734C96EDAD17A986467EAB507961CA3784833C280D7EB07D6345FA10FD860
            10D37E2636718011ED7692E20338818BC5DC21A5F8CEA528FA8005F0F970C205
            D8024B10DE6AF37F001FA4B711F09A65FB0000000049454E44AE426082}
          Visible = False
        end
        object imgUniGuiMobileYes: TImage
          Left = 85
          Top = 68
          Width = 16
          Height = 16
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
            001008060000001FF3FF61000000097048597300000EC400000EC401952B0E1B
            000001C54944415478DA8D934D2883711CC77FCF339E36EFCAD21C48C2453908
            879D0C279C266E23070707A51C0CAD4C58D9C65971D0949D8C7693CC4B997263
            E580BC84421407129BEFEF99FF3CAD95FDEAD3FFEDF7FDFEDFA5582C46DA68F7
            3B6B50F48366500638E1126C828560B7E3549B2F09030815141E3000644A1DDF
            601E8CC0E82B61F02B0E82564A2F3680954D8481171D43698A454CC1605C6A5B
            9DA846230274FF298A0C7954A0CFA1B3973B6E7E822A36984565F83FB111E269
            4B0FE52A7A1A0BF9E8FCE59EBB27D9E008953A91284B1245936EC668C887D846
            A69C428C45C97D10A0DD9B131EDA6783475E1DB71A4C95D45BDB428E9D157A7A
            7F4D885D101743FC1D8DD2DCE13A85AE8F85F7351B3C705E9E92458B1D8364C8
            50E8EEED99ECDBCBEA6A5C4D7F626F38403BF199455CB1C1214FCEADC6926AB2
            9B3B2943D6A9263A4986B840157BC289656B23C40653A88C8A9E865F934C397E
            2971F11AC4915467AB5E63392AFC3C958409CEC26EEE52B7E086782FB5F81D54
            888734C96EDAD17A986467EAB507961CA3784833C280D7EB07D6345FA10FD860
            10D37E2636718011ED7692E20338818BC5DC21A5F8CEA528FA8005F0F970C205
            D8024B10DE6AF37F001FA4B711F09A65FB0000000049454E44AE426082}
          Visible = False
        end
        object imgUniGuiWebNo: TImage
          Left = 85
          Top = 47
          Width = 16
          Height = 16
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
            001008060000001FF3FF61000000097048597300000EC400000EC401952B0E1B
            000000D94944415478DA63FCFFFF3F03258011D980B3BE2681C69BCFACC7A701
            A8260CA86615860140891020B50C883F021588E2D0FC0C488901713C50CD52B8
            0140097F207B3510B342D5BE413704AA5912CAFD0BC4B14035CB915DF01848C9
            20E9811B82A69901EA4A016C6180610810FFC6A519C3001C8630E0D28CD5003C
            866068C66700BA9FB1062C2E2F60D38CD310F440C4086D20FE02C4D2B80C418E
            469C51852F8A6109A90CC8EEC41760580CE907AA294276410B90AAC615DA6886
            CC07E214A0BA7FE8615003146C61C003806A402EAD0469C6198DA400002CDFA0
            E1A49CF2280000000049454E44AE426082}
        end
        object imgUniGuiMobileNo: TImage
          Left = 85
          Top = 68
          Width = 16
          Height = 16
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
            001008060000001FF3FF61000000097048597300000EC400000EC401952B0E1B
            000000D94944415478DA63FCFFFF3F03258011D980B3BE2681C69BCFACC7A701
            A8260CA86615860140891020B50C883F021588E2D0FC0C488901713C50CD52B8
            0140097F207B3510B342D5BE413704AA5912CAFD0BC4B14035CB915DF01848C9
            20E9811B82A69901EA4A016C6180610810FFC6A519C3001C8630E0D28CD5003C
            866068C66700BA9FB1062C2E2F60D38CD310F440C4086D20FE02C4D2B80C418E
            469C51852F8A6109A90CC8EEC41760580CE907AA294276410B90AAC615DA6886
            CC07E214A0BA7FE8615003146C61C003806A402EAD0469C6198DA400002CDFA0
            E1A49CF2280000000049454E44AE426082}
        end
      end
    end
    object wzPageInstalation: TJvWizardInteriorPage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'Instalation'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Subtitle'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      Color = clWhite
      OnEnterPage = wzPageInstalationEnterPage
      OnNextButtonClick = wzPageInstalationNextButtonClick
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        641
        394)
      object mmoLog: TMemo
        Left = 11
        Top = 72
        Width = 621
        Height = 270
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        ExplicitHeight = 228
      end
      object btnInstalar: TButton
        Left = 397
        Top = 355
        Width = 235
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Instalar'
        TabOrder = 1
        OnClick = btnInstalarClick
        ExplicitTop = 313
      end
      object pnlInfoCompilador: TPanel
        Left = 11
        Top = 15
        Width = 621
        Height = 51
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvLowered
        TabOrder = 2
        object lbInfo: TListBox
          Left = 1
          Top = 1
          Width = 619
          Height = 49
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 14
          ParentColor = True
          ParentFont = False
          TabOrder = 0
        end
      end
      object btnVisualizarLog: TButton
        Left = 12
        Top = 355
        Width = 157
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Visualize Log'
        TabOrder = 3
        OnClick = btnVisualizarLogClick
        ExplicitTop = 313
      end
    end
    object wzPageFinalization: TJvWizardInteriorPage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'End'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Subtitle'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      EnabledButtons = [bkBack, bkFinish]
      VisibleButtons = [bkFinish]
      Color = clWhite
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbl13: TLabel
        Left = 40
        Top = 37
        Width = 293
        Height = 23
        Caption = 'Installation completed successfully!'
        Color = clGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lbl14: TLabel
        Left = 40
        Top = 112
        Width = 134
        Height = 16
        Caption = 'For support contact:'
        Color = clGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lbl15: TLabel
        Left = 40
        Top = 134
        Width = 183
        Height = 16
        Cursor = crIBeam
        Caption = 'suporte@falconsistemas.com.br'
        Color = 11103753
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lbl16: TLabel
        Left = 40
        Top = 176
        Width = 162
        Height = 16
        Caption = 'Falcon Sistemas website:'
        Color = clGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lbl17: TLabel
        Left = 40
        Top = 198
        Width = 162
        Height = 16
        Cursor = crHandPoint
        Caption = 'www.falconsistemas.com.br'
        Color = 11103753
        DragCursor = crHandPoint
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = lblSiteClick
      end
      object lbl18: TLabel
        Left = 40
        Top = 244
        Width = 373
        Height = 16
        Caption = 'To stay up to date or purchase new components, access:'
        Color = clGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lbl19: TLabel
        Left = 40
        Top = 266
        Width = 204
        Height = 16
        Cursor = crHandPoint
        Caption = 'https://store.falconsistemas.com.br'
        Color = 11103753
        DragCursor = crHandPoint
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11103753
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = lblSiteClick
      end
      object lbl20: TLabel
        Left = 40
        Top = 64
        Width = 287
        Height = 16
        Caption = 'Thank you for choosing Falcon Sistemas products.'
        Color = clGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
    object pnlTop: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 818
      Height = 65
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Color = 11103753
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 7
      DesignSize = (
        818
        65)
      object lblDesc: TLabel
        Left = 16
        Top = 8
        Width = 362
        Height = 23
        Caption = 'Installation wizard for Cod Lab components'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblSite: TLabel
        Left = 16
        Top = 37
        Width = 62
        Height = 13
        Cursor = crHandPoint
        Hint = 'Site Falcon Store'
        Caption = 'https://store'
        DragCursor = crHandPoint
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lblSiteClick
      end
      object imgLogo: TImage
        Left = 747
        Top = 3
        Width = 60
        Height = 60
        Anchors = [akTop, akRight]
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000FA0000
          00E60806000000FCF898DD0000000473424954080808087C0864880000000970
          48597300002DFB00002DFB01FC874AE70000001C74455874536F667477617265
          0041646F62652046697265776F726B7320435336E8BCB28C00002B1F49444154
          78DAED9D079C5C55F5C7E7DDD7A66C4D210524A4928412124257822035028222
          4D02D205041BCA5F041110440529022248171490123A4A0D48912002A12524A1
          252165B36DCA6BF7FD7F7733D1B03BB3FB66F7CDDC37B3E7FBC97C263B6FE6BE
          73EEBBBF7BCE7DE55EC5F7FD18517B2C5CB262F87FDEFFC4F9C6DE335B65DB42
          C84721A1D726ED9DD9CD0D5DBBD8F1BC23EA93F19C6C7B08B990D06B949C658F
          304D7D85CF637731A61C2ADB1E422E24F41A65D5EAB6C4B0A10DEF288A32C6F3
          F8F755955D2EDB26421E24F4DAE645BC76C431F65C8FEFAA6BEA3F651B44C881
          845EC3E0D83E8988BE7BFEFF1F652C67EB54DC68936D17517948E8358CC7F943
          2A63B3D7FFCDB97F2FC6EB5F976D17517948E8350CC6E6B7636C7EE4869F2185
          3F4E53D98DB26D232A0B09BD86E19CFF8E31F6BD0D3FC3E1EE745C77BAA16B8B
          64DB47540E127A6DF363BC2EE9FE2152F8E790C2CF926D1C513948E8B5CD6978
          5D556883E779A7ABAA7A5589E511550A09BD8641E43E1591FBF785B6E1B8A76D
          C79B621ADAC7B2ED24CA0F09BD86713D7EB2A6B26B8B6D474730171DC181B2ED
          24CA0F09BD86715CEF785D53AFEFED3BAEE71DACA9EA7DB26D25CA0B09BD8681
          D0BFAD69EA8D4A2FDFC1F15F92CED85BD6A5CC8C6C7B89F24142AF6120F4A311
          D16FEEEB7B1EE7BF50193B4FB6BD44F920A1D73010FA1C08FD96BEBEE7C7FC8C
          6DBB934D43A71373350A09BD8681D08F41EA7E9312E0BB9CF3DB196347C9B699
          280F24F41A06423F1642FF5310A103DF76DC1986AEBD2EDB6E227C48E8358CEB
          79276AAA7A5DD0EF23AA3F84A8BEBF6CBB89F021A1D7301EF74F5199727529BF
          711C6F475D575F966D3B112E24F41AC68FF9A72AB1C277C6158373FF11C694D9
          A5FC86883E24F41A0687F6278A12FB55A9BF4354DF1651FD35D9F613E14142AF
          613CCEAF5419FB6EA9BFC358FD2E8CD56942C91A82845EC3781EBF4B55D921A5
          FE0E2DC2751C770A3DB35E3B90D06B188CB79FC0787BCFFEFD965F8AA8FE23D9
          3E10E14042AF6D5EC06BE7FEFC10ED6275CE7626244C9A4CB21620A1D728AB5B
          3A53439A536F3345D9B4BF65789E779CAAAA34BF5C0D4042AF512CDB198531F6
          624551E2FD2D03A9FF8B48FDFB951110D182845EA3781E9FA6AA6C40B7B3A269
          F88EEB6E8D0EE32DD9FE100383845EA338AE7798A6A97706BCCFBD289CF38B18
          6367CBF687181824F41A85FBFE99189FFF7AA0E5A07D2C5CFAE9EA29633719EE
          C9F689E83F24F41A0591F82F61DCF4229A87EB793BE89AFA8A6C9F88FE4342AF
          41E6BFB14499B1D566F31545991E46799CFB1732A69C23DB2FA2FF90D06B909C
          ED6C64EADA52083D1146796823FF46593364FB45F41F127A0D8254FB2B9AAAFE
          3DACF2D0445CDB71C454531FC8F6AD12A433D678BC8D4F25CD2764DB121624F4
          1A04A9F64F916AFF32CC323D8F7F5B55D9CDB27DAB04A8BF1BF0D6803AFCA66C
          5BC282845E83E0983E8E547BAF30CB44E3BF190DFFDBB27D2B371D995C7D5D32
          BE2AE6C75E5094D81EB2ED090B127A8D91CEE69A937153DC11D71466B9DCF717
          BCBFE4B3AD268F1B59D30D0699CBF791B95C868E6DD11D77BF32F15B87EE20DB
          A45020A1D718189FEF8BF1F92361978B6692C3387D22C6E99FC8F6B15CAC6D4D
          C79B1A930BD1496E823FD7C0E1F17143AF89877A48E8350622D1D548B14F09BB
          5CD14C38E7FB21DA3D2ADBC77281687E0AFCBB7A9DBFBE6FD9EE7671539F2FDB
          AE3020A1D710AD1D69BDB1AE2B228D2947F91EE73F51191BF0DD7651A4BD236B
          D6D7C5DF5B5F774215E8D87687BF4FCBB62D0C48E83584E37AB390B63FA30CF4
          06F722609C7E2B5394A365FB590E44346788E61B561DB2A323901DDD29DBB630
          20A1D710E2B2101AE671E52A1F6DE53944BC59B2FD0C9B8E742E5E9734DF876F
          5FD8F073D7E5E76A1ABB40B67D614042AF113A3356632A612C41636D2ED73E10
          D13FF96C75EBF851C39B6DD9FE8609A2F90F3036BFB4C0E757E2F33364DB1706
          24F41A018DF27834CAEB075E52AFAC753DBE99A6B276D9FE8645673A5B9F4AC6
          C5798D110536FF01AFEFC8B6310C48E835028EE3FC72DF8F8EA6E2D98EBBB569
          686FCBF6372C3CCE7FA532F69382FEC6627761CC5E13D35E93D06B00C7F5F6D0
          35F51FE5DE8F682BB6E3ED06A13F2BDBE730B06C77534357DF4107992CB49D73
          FF19C6942FCBB6330C48E83500C6CE8F3245D9A7DCFBC93F9BBE1F3A959AB896
          0E21DF06217FABF876FE1A636C5BD976860109BDCA4184DD4E4C0A51AE4B6ADD
          41E33F008DFF41D97E0F14D4DBCE88E62FF4EEABFFCEAFAF797CEA59A795BD0F
          2D3B24F42A07D1FC1144F37D2BB63FEE1F88283857B6DF03E1C997DE8FEDBEC3
          44748ECA767DF8BAE889796F4DDA67D656552F12127A1553A9B1F986D482D03D
          8F7F4755D9357D7D0FDA58B2F4A39513C78E1951F5F3E591D0AB1844F35711CD
          2B3A86ACF6D4DDB29C9186A1BD1DE47E036863E9F2E56D13468F6E22A1137270
          3D7EACA6B23F55729FF99371FB228B784CB6FFFD0519C99DC8480E0BE6AFFFE1
          F296F4F8D143EB48E844E5C959F650D3D0DF45541A56C9FDE62FAFCD320DED39
          D975D01FD0391E88CEF1FE12FC5DBA6C65DB848D475044272480F4F966A4CF15
          7FB844CC1DE7B8CE1686AEBF2FBB0E4A253F21C702748EA382FE0643A3258B96
          7E3661D2D8915CB6FD0385845E65B8AEB797AA323155948CDDB7388EBB99AE6B
          1DB2EBA154D0392265678152F6FFFDC65FF8E8F30B369FBDEB96552F12127A15
          D199C9D6A712F105DD9FB2AA1468F88B177FBC72F3096346B8B2EBA2143CCF3B
          4655D59BFAE1EF028CE7B7946D7F1890D0AB0834BC5BD1F08E92B57F34958791
          487C55763D94826DBBE39181BC0EBBEB4AFD2DEAFB15D4774D4C1A4742AF125C
          97CFD134768B4C1B3C8F5F8561C3E9B2EB22282B96B56B1B8DAA7F99F5F3611F
          08FD3108BD6237239513127A15603BEE145DD35E45544A0EBCB4FE03A1CF81D0
          6F935D1F81EDE5FC3A95B1130750C4CD78D5C414D724F488D39EC99A7589F8BF
          1095B69269877844D571DD6986AE2D905D2741D870A2C7FEC27DFF1AD4FBA9B2
          7D0903127AC4E19CDFCE183B52B61D68278B97AD6C9B540DD7941DD7DB5B53D9
          A3CA002F4DB8AEF72B4D53FF4FB63F6140428F30884A67222A4562D6558C576F
          C178F518D976F40544BE0544FE0234DE38D0B290FA9F88D4BFDCB3F65404127A
          444134D91F229F2BE97A794F7B3C7E1804F457D976F446CE7236310D6D1EEA6C
          B330CA43EABE1752F7D016AB9409093D82D8B63B59D7B597A0F10147A530401B
          69CB5ACEB864DC68916D4B31D2596BA344DC781AC29C1A92CF380EDE0EA6A9BD
          22DBB73020A1478C9C658F340DFDA5722DC2D01F90B6DF85B43DB273A7A5D3D6
          B044C27802364E0FB1D8361C8B0971D3582DDBBF3020A14788356B2DA3B9D178
          1E0D76BB8197161E18F746F689B54CD61E11377571BD7B9B30CB852E3E78EEB5
          8513666D3B49B68BA140428F109CF3FB18635F936DC786A07D2C6CEF484F6D6C
          A88BDC6DAF88B89B1A7A97C8A794C1EF0791551D20DBC7B020A147048FF33FA9
          8C1D2BDB8E1E7679FC7BAACAAE906D47775CCFDB0EF57577B98638E8742F43A7
          FB43D97E8605093D02600C7C31A2D259B2EDE80EDAC69AACE54C48C68D56D9B6
          6C08861207692ABB15222FF9FEF5A0A083FB163AB83FCBF6352C48E89241E438
          0391E372D976140259C6CF1135CF976DC7E76CF2F8CFD1299E57CECB8E9084EF
          B8EE0C43D75E97ED6F5890D02582F4F3244D55FF20DB8E42A05DAC40349F8C68
          DE26DB1641D7E53353BF169DE2C115F07D794747766C4343D292ED775890D025
          E178DED11A633747E58698EE20729E84D4F58FB2ED10B81E3F0099C5D5A8AA4D
          2AB1BFFC14DAB365FB1D26247409B8AE7788AAAA774554E3A2A1BFF6FED255DB
          4D1EBB91D42994B259BBD934B55F298C9D58C9AAC270EAA7C81C2E96E97BD890
          D02BCCBA5B5BD57B21724DB62D85406BF061E38E62F5179976208A1FAA32E512
          19370E39AEB733FC7F51A6FF614342AF2068BC7B692A138B1F98B26D29467E75
          51694F6CD98E3B5D53D50B1953F693B17FE861593A638DAF4BC573B2EAA01C90
          D02B04A2C4F68812CFE0BF09D9B6148373FFF5159FAD9D397AD4908A3F8A6A39
          EE4474826732851D8F6C47DAA0067570BBCCE9BACA0509BD02388EB7ADA6A9FF
          40036E926D4B31D00C32B6E34C378DCA4EE58C083E4955D9694C518E2BB67C71
          2541D6F50D74387F936D47D890D0CB0C443E5DD31844AE0C916D4B31440BF05C
          EF7074467FA960BDCC5055E524456173D001C665D741573DF8B1B539CB1E9B88
          C825C53021A19711DBF1B6D1D7897CA86C5B7AA352E3F26562B2C611A9BD1963
          A7203BDF2F6A571D90B6DF81B45DFA6C3EE580845E266CDBDD4AD7D52721F2E1
          B26DE90D34EE07D1B8CBFAF0866561FCADB1438588501F9365FB5C0CD7F3F6D3
          54F551D9769483B20BFD82ABE66AE3371DAE8A7DC97676A00C1F5ACF268D1DA5
          8FD97868AFA99D658B658B3421F211B26DEE0D1CFB45999C3D339530434F55B3
          96DD6868DADE88DA47203DDF07EF91BDD290AF8BC56B3BB2538634246DD9B694
          83D0849ECB39C3558DEDA032651AFEDC01E9593DE7BC81FBB114F611C96BC6A5
          A088BA8AC5128EE39E914C987717FB9ED5353B8CFA245394D1B26DEE0D1C939C
          EDBA334D5D0F6D56D77426576F9AC66EF0FD60747242DC2365FB19140C5FCEC5
          F0E502D976948B01093D9DB652665C9F8D037B380EEC2CA8A1B9EAC3762FA0AE
          2E839F451F5DB42C67A2AE6B4F2145ADC8AD9A03C175BD23354DBD63A0E56432
          4EB369AABB8967B721EC3DF1BEB16CDF4A05C735633BEE04D3D097CBB6A55CF4
          4BE81DE96C53326E9E82067DB2AC75C02A0D7AFC6BD1E39F526CBB98532D6E68
          4FA33E36956D6B005F2E872FDFEFEFEF91D56CC254B6BB1253F687BFBB42E01B
          C9F6692070EEFF116DF924D97694939285EE79FC3854CACF078BC005DCF7E722
          6B39B0D8F64CD61E138FEB6262C2B1B26D0DE0CB33B0F3CBA5FCE6C3652DFAE8
          E18D3399AAEC81E3BE37B2B66DF11ED91B7F4A01EDDF711C6FAA61688B64DB52
          4E020B1D69E918DDD0FE8846B2976CA32B09EAE73F1D19EB8B0DA97867A1ED39
          CB196DE8DAB3E8FC26C8B635802F2B2CDB9D1E37F5157D7D376B3B1BEB2ADB05
          82DE4B0CCB10BD2744ED7258182070FD4955D9F1B2ED283781849E5FF942CCE8
          51D5295AA98819566CC79B611ADA4785B6235D6F34756D1E442E75B9A460BEC4
          7CCFF3BE8271F95385B6BFBB7099367EEC46DB33C67683A8BF02516F57CE195C
          A200EA24673BCEE6189B7F34F0D2A24D9F42E79C9FA830F607A5062E8F958AEB
          7A7B41180527F05FF659AB3E72A3C64791E1EC21DBCE20E4577DF96DA16D2DAD
          69BDA921F90484BD5B2D46EDA275C2F9F92A633F976D4725E855E8A888D3D0C3
          5F35888EFDFF7CF7F83910C685C5B6A303BC0775F375D976068173FF3E641D45
          6766C171BE010DFE38D9765612B4FBA5999C353595886765DB52098A0ADD13CF
          03ABAC62F73E470988F8EF10F15EBD6CBF18DB2337996321707C97608831A3D8
          048FAEC74FD02232934C2541B6361BD9DA23B2EDA8140585EE38EEF6A804B186
          9521DBC04AD3F560836D6F95308D4F0B6D775D7EB4AA2A919D02EAF3BEF81E84
          BC8BAEA92F17DA2E1E9D85C89F872FBA6C5B2B89C7FD9B54A6446E6AED72D243
          E8E99CD590348DD770F0C7CB364E0610C65168FCB717DA66DBEE74DDD05E82C4
          ABA2034456F67D6465056798B56CA7C9D0B5F938CEE364DB5949448693C9D85B
          A552665AB62D95A487D007E3786D3D18CB3E84B1ECFE85B675A67375A9A4293A
          C089B2ED0CE8CBFDF0E5A0E2DBF9BD187E1C544A99D50E9ABAE7BAEECEBA5E1B
          0B2796C2E784EE38DE4E48D9FF59055969E8A01AD2B6E34E2D76290DC2B819C2
          385AB69DC17CF13FCDE4ACAD538978C1D54F11E97F8448FF1BD976561A646BA7
          225BBB46B61D32F89CD0B9EFBFC0146567D946C9008DFF6C34FE8B0A6D13B3B6
          A203BC4BB68D4110C7130D7A778CCB9F2EB4DD76BD5D74557D0E9D39936D6B25
          41867325329C3364DB218BFF0ADD71BD3DD1989F1884C1BCEB11C5B6F6CC94A6
          C6548F471433397B48C2D4DF8DFA73E5EBC1D0EB020CBDCE2DB42D93759A1371
          4D0C3F36936D6725C95F0A3D44B61D32F9AFD0F313107C55B6413240043C1229
          5DC127B9AA2965C731144B2E7FA9F8F6C1372E479D3CFAC9AAD5076E3A62B823
          DB16997409DDB2DD4D0D5D5D38382FA7F96F2E787BD9B42DB7D8B8C775466439
          BB22057E56B68DC1FC88753AAE3BCDD0B5C585B66368727A1457452D27C86E1E
          696FCB1EDCDC9CAA99A595FA4B97D0D1084E4623B856B63132703DEF304D55FF
          DAFDF3BFDEF552EC904376F817539499B26D0C8278AA10C7F0C642DB6CC7DD1A
          1DD6AB83E97A39B2973B3FFA74CD9CCDBE303C72EBBACBA04BE878DD83465015
          B7738609D2BAF796AF6ADB62E3114D3DE631473A8F0E80DD29DBC6807EDC8D94
          FD9B85B6B5B4B69B4D0DF5FFC6F629B2EDAC047E577DF08B54C6CE966D4B9450
          3259AB296EEA2F43E893641B53691005CF4014BCB2FBE72B57B76BC387D6BF19
          E5890CD72356FECC59D6968978E14B6968F46205D29365DB59A1BAC8205DFF2E
          32B41B075E5A6DA158B6F3355DD36E519458836C632A091AC5DA9CE54C48C48D
          1E02A9A6682ED672D334F5A182DB3C7E303AB2BF0D862B29389EAF3B8EF76DC3
          A89D35CDC344F13CEF5CF4F8E7C506D963A8887437C1EF1EF73BFFE2D28763E7
          FC60BFF9189BCF906D63DF3EF83722252F7817233AB151A6A1BD81AC64986C3B
          CB89187A72DFFF6D5B5BE69C21CD7535B55E5A58AC6D4B4F571CD7BB02D1EB74
          D9C6541A44C2DDB40267D41DCF9BA5AB5D6BA4451A34F08FD35967CBBAA4D15E
          683B1AFF83E8AC6AFA72A9B8C10B81EA27C8485F906D4B14C95AD65043D7AF43
          4030C518FD7EA4AF070EBCD8EA413CD8B07A6D7AE2F021753D4EC2E517D98BF4
          6A1DE2D60734F07DD1513D56683BB69DA8AAEA75B2ED2C9FFFFE221CA74B302C
          B941B62D51C5118FE1AEBBDD7784EBF26D94CE8CF5542A6194345960B583B4FD
          1AA4EDA776FF3C6339431386B604E96EBD6C1B7BB7BF78CA6E3BCE0444B8376A
          65F2C60D111D347CBFB6339DBDA6B12135A89E3E0B4A4767B63E99302F42FB3E
          4D3CB382FABA016DE504A5339D9B974A9A5F946D602541DA7E00A2E183DD3FF7
          3C7EA45AE411D5A820D6EFCEE59CA98944E185007160C51C7635753CE1D32B7E
          CCFF537BA7F5E7E6860409BC0862EA33A6B22B58FE6A91C8FC5CAFEBA6AF7983
          4EE870BE3D9BB3C726133DCFB663CC77372AE91BB26D2C6A7BACEB601E8C0377
          5FA1EDF9E7CF2F936D6728BEFA7E2B5E73C52411F975E58922746672CD187E5F
          C81476CA864F9EA2FEDE7AFC89D7B7DE67EFE9FEA0133AC43C0F62DEB54765A5
          7329D4C322A4BC915D460891ED6F88D6053B22DBF12642106F446509E2FE2022
          1022F7F33EF7EFB41CEFDE64BCEF69A9073BE2CE4E95B18B94026B0AE4EF78ED
          3A5733F884CEF9E5ACC02A258EE3EDA2EBEAF3B2ED2B0644D061DBCE54D3D43F
          29EC972FE696DFB5D472A38098565B645388DE371B45A6BD223E8FE538137555
          FB0D8E79C113E9A8D3CF3A3A73E31BEAD70D75069DD0D1980E5799F2979E9F23
          ED65D14D7B5D8F9FAEA9ECAA823E79FC54F4DCBF976D63A988CB6388DE37237A
          3F80E8BD4AB63DD5407B7B2E914A193F82C07F82289E2AF63D74FC17E23BE7AC
          FF7B50095DDC5CE1B8DE8E86AEF5881A88F47720D21F2EDBC622768B67C8B72D
          B4CDB29C4D0C437BA75A165B802F6D380C0FA273FA2332A879B2EDA9263CCF3B
          026DF45C1CEBCD7BFB9E982DC9B29D8971F37F8B460E2AA18336CBB2279AA6D1
          237AA001BE5A4C4CB211537C41142F15DA869EFBDEDEE6868B02E22422EAF71D
          44EF1BD1D1FE79C30648F48DEBF22F8BF50EF19A15E4FBC85AAF54BBCDA633A8
          848EC6F6E187CB5BC66D367A28DFF0F3B68E4C53435DE2BD282E390521DFC48A
          4C4D8C74FE200C37EE8DEA1C7F5D27D77CFF11A4E837AD58DDFEC026239A06F5
          E40FA522B24F1C5F91A27F2DE831469D676DDB99D4FD5CCEA0123A44F3EF63CF
          FCE38C9B2FFDFC0AB9B6E38ED3B5AE893722358F1A0E5A6BCE7226250A8C5F3B
          D3D9442A191729FB18D97616B01BE9B97F1752CDEB755DFB976C7BAA0D64703B
          AA2AFB81C294434AEDC30B4573C1A0123A1A9F58BF7CF7029F4FC4E7EFCBB6AF
          3B1EE767A2472FB85E1AC6B8E216D01FCBB67143508F1F217ADFE8D8EE0DF178
          E105308A71D9EFE72AA79F3C7BBF9812D31C87BF82CE6DD0A5F762DE46B5EB8E
          36E580FE6469BEB8B9D3EA1A9B2FEBBE2D7242478F244E94B5AF1BD9858A0177
          1F446F7769F70D39CB9E8671FBEB51CA80219A0F3A3AB2531B1A923D26AC741C
          7773ADEB9A7934A6FE82AD6FA0E3B92A63D97736A44ABB734D9C4C842F876078
          720CFCD93A5F9E3861F726DE9FC3FB738EEBFC3B113757CAF6B31CA433562384
          7990B84D35A6283B0FA40DF6363168A4842ECE8A6773CED464C278A792FBEDCC
          E4764E25E22F4469ACEBBAFC1B9AC6FE56681B86208FA161EC2DD3BEFC09B6A7
          7DCEAFFACFA20F1F98B1F9385ECAEF11BD7641A33C11757E505FCF16603F2D68
          1A6FE56FA69967BBDEEBC9B851D537D3A0B39EC15476245394C3E0FFE8819687
          3A5AD1D1999BD4509FE828B43D7242B71D6F5BD3D05EABE47E3BD2D92FD525E3
          CF45653D35A4BF2FA101EC54689BEB79FB6BAA3A57966DEBA61E8B3DC87D7E05
          EC78B294DFB667720D29D3F826EA790E5E5FEA6F75E7CF01BC8DCEE6C598CFE7
          392E5F80A8B850569D04C5462686E1D66C1CDB8321BD5DC26C6EC8A84EE96DDE
          C7280A7D26843EBF92FB451DEC944AC623B1428D88949ECB774734EFB100C327
          CBD7A8A3470E798B4998E26ADD19747E0FB289DF89D57C4AF92DA2F738642073
          60F7B110F817C2B7CD1767F317C3C657D049BE814F5EC13EDF4B98C66795AEA7
          0D6969EF4CD427E2D35555DD136D6B2FC86D7BBC6B61EF073EBFFEF1F2D533C7
          8C1EEE15FB0E093DD6351BCB344484484C418483F61804B16FA16DA2D746BA77
          7525FBA3FC25B2BF60DFBFD375B5A435CB20B69D55A69C22261EC5AB62F7E0E7
          CFEEB422CDFF04C27ACDE3FE074C89BD6A3B7C99E2FBEF1F72C135D9B9BF3C23
          D493404BD1098F1C523F5AD3B4CD717C7682BFDB61DFDB4262A3CB1D405CD7FB
          B2D6C7833F24F458B4CEBA3B18BA40503D862E96ED361ABAF61E1ACD88CAD449
          57BDDCE571FE5B5D53035F223BEFDA07949F9DF0D5AF238D3C35A628BB452049
          DAC027A16D658578320EF5F836DE57224359C9187B171B5A3CCFCBA15348C714
          3FADC4940C6C77E083879E83A10316CFF7D789F309AAAA2454C6EAD0F96D81DF
          7E21A6C436C177A7A2EC8DF0FF54257DC6F1B90DB6CCE9EB7B24F458D772C863
          F5750B58A832FD47A37B0029EED70A6DCB9F51FD59B96DE83AC9C6FDB962CA64
          AD84074C3A3AB30DC9A47914EAF0786424DB54B0DAC2F3DD5F5F0331A4C08A0D
          D1BA62EE06FCCDB04DC7B67559097A892874606282530480A9C846FB3C314942
          07ADED99C6C6FAC4BBB21F51450AB6632171E572F670D334D011C51ACBB97F44
          ADA7D0A1FC5257D5A782FE2693B347E0781D0B719F38D8D674938DEB79C76AAA
          7A5390EF92D0FFB7EF57C4B84AA2EFFFC0FEF72CB40D29E26F900AFFA85CFB86
          C0DFE29E7F81A6B1C02BC65A96F3054D574F82C04F88E2ADC3B54EFE12EBBE41
          BF4F42CF237B524844F33D10CD7B44D2ACE58C881BBA88E6A1CF6327AEBD42E4
          172EFE78D57513C78C08B474118ECF78743AA7302526CEA037C9AAAFC10C8E5B
          3B8EC356D0C947417F4342CF935FB5E572497EFF0BA2D9BEA05D9C5F82B179A8
          B7BA627F2E047E956DBB9724E2C12E41D98E2B04FE3DA630083C9694514FC43A
          5C97CF41F6755B29BF21A1E7119782744D95323F383A996F41447FEEFE79CE72
          86A22E1685193921F047913DFCCCD083DD9494B39D4D0C4D3D5D51D8C9E5C82A
          88D2F0B87F87DA8FCC93849EA73363A552096371A5C79BF0F9938EB435A9A12E
          9EEDBE0D1DC059E8002E0E633F5C3C70C2FDB3B580B3DCA6B3F6B0B8A99F8E14
          FD54D4C9904AD60951181CC30F32696B9BBABA7867A9BF25A16F407EF2C5832B
          BB4F7E012BF020425B67C6684825DE1FE863A8F95B56AF46643E2F193756F7F5
          FD152DEDE6F0C6FAD3188BFD10FB1E55C9BA208A232647723C772743EB9F3648
          E81B80083A0711F4960AFAEBC0DF29F0F78302B61C51289D2F05448005DCE367
          685AB07BD25DEC53CC3326E3165BA2775CCF3B4153D57EAF4C4342DF809C38C3
          6D6A8B512D1539D9840CE289624FA141A4CF4370BBF4A7DCFCC28397B5B665CE
          1E1A60E141D7E53B3155B9503CAB1F851B4188CF8371F9EF312EFFEE40CA20A1
          770302B91702ABC81C6C88DA47155A19C671DC6D1085C5849025EB0E75B8D8F5
          FC93748DFDA3AFEFE62C7B6343D7CE57D69D49272288B85EFED23B8BF7DB798B
          F103BA379F84DE0D44B703348D3D50015F5B2DCB19178F1B6BBB6FC3B8FDB242
          73CFF785C7F9ED8ECD4F8FC7B5B57D7ED7E3A78909076B7D59E56A46ACB492C9
          DA3B419F259F7CEB0E09BD1B2D1D9D7A735D4ACCC536BE9CFB414F7D0F847648
          F7CFDB3A2CA3A14EDCEEAA6C1AB42C549B85CEE107EABAD5337BC571BC1962D9
          A6A0338A1272106BEC59B6BB4BDCD49786511E09BD00888C3F2C36575B58B81E
          3F5C53598F85245CD7DB1369FB1341CB11534E210B395CD77B7FC2ECA3656BB4
          8D47369F8B61C959E844F472FA460C0C1CD355D0C16ED0C1DB619549422F40CE
          B68798BAB8EDB43CD78FE166A7653BE3D15BF798070D91F93AA4ED2706290759
          C1A396E3CE49987AAF97CD1CCFDB4E63ECF7C5EEBE23A283589ECA71BDAF18BA
          16EAFC0824F42220AAFFA2D8447B21F8F90C44D7634DFAD6B6ACD1D810EFF3DA
          F9BA4749F915B3BEF9EBEFCDBBE7ACDEFDF0F84F91A69F47513CFAA05DAC164B
          1FEBBAF6EFB0CB26A1172197B39B4C5317A21B1E76D910DFCF314E3EBFFBE7E8
          C977D2FB98A6493C338DDFFF58D3D86F7AB5DF76363534ED46887C8FCAD71E51
          2AE20123C7F1F6360CED8D72944F42EF0508EA7408F28A707DEC5A436B778CC3
          7BCC09872CE2A7C8227ED9DB6F91DA1F0B9B7A7D06D971F96C4D556E90FD7C3D
          110CEEFB8B20F2D968F7659BE58884DE0B2D6D69BDB921F5A6A2C4361F7869EB
          7D14ABAFD8131271634DF76DF9F9E28A4DE3EC40E44761FCFED7DECA47E7744E
          A16C8188265C4C5FEDB887967B3D3A127A1FB82EDF0BC2793CAC1B4A20E6D758
          81C51CDB3B32A9FABAC4A222519843C0FB093B8A95BBBA259D18D29414EBB41D
          2ABBCE886088F5E017BEBFE2F8C9934779032FAD7748E8011037A220A50E6552
          0AF87813C4DC63D1448CCFB7C6F8FC3F05F7EF79DF5255B5E87DEF999CBD51DC
          D0EF87C8778A119107CD1C4D8A7F5F2DB2DE7D3920A10720673943C4354D0874
          C033B0BA1E3F5753D9053D3E77BD23316EBFBDC0F74FD77A691018DB4DD134F6
          60B96FF021C201A9FA3BC8CE4ED0F5CACE7D40420F0884F83508F1BE819683AE
          5C2C757C7FF7CFD100CE41443EBFDB77C5EC3245AF9F41E4D321F2C768CEB6E8
          D37522D5F7AF4E67ADB31A52A53F4F3E5048E8258074EB7AC6D8F103F10F82DE
          0129DB2B05B689B3E4C7FD6F5FFE2310FEEC6265A19E66E81AFB3B4D0A117D70
          2CFF8328FE2344F13E1F342A1724F4124867AC78226EFC0B02DCB29FFEF996E5
          4E8BC7F537BB6F43F47E06D17B56FE7B1F67B2F656382E6D85CAC1787EAAA6AA
          CF2A4A8C1E488930E2061888FC579F2E6BBD72D32F0C7164DA42422F91752263
          626AE8543FFCCB6473CE9464C2E8317B271AC47C742033F2FBF8B25E64899D9C
          650F370DFD65EC7F6C18FEAC5FB420FF446C564C1C898F2C7C24A6B64AE363BC
          2B59BC67F077F7259CC58F52F88D58D8208EB2C46A26E215C71603656AF8828A
          F2CCF5EB240D86C761C5B2CF48D3AF771CF7F2B859DA3AF1E58284DE0F5CCF3B
          1811F56FA5FE0EEEADCEE5ECCD1309A3A5C066716FF3340C0F2E6145C6E56BD6
          761ACD8DA979E8104ABA675D881902F3C4A3B1F87321FE5CEE73FF5394F3163A
          980EA4956BB1FD535DD7565896C32DB4D05CCEB157ACECB0B79EBA49A069A0BF
          71C2C5B18BFEEF58BDA9216E18862E50E32606298A62382E1F0923862A4CA947
          D612C7BEEAB0DF09F8D908D8500FEBEA61DB187C3E049D83097BBB26FEA8B64E
          41646210F86D8EED5D87AC2DF054CC958084DE4FFA73630ADCFB2C2FF44229F9
          62F86FAE6DEB1C37A4A9DE2AF47B88435C273F26C8BED0E096228ABEEEC7FCF9
          3E8FBDCD7DFE96E5B89FD527E36D417E5F69167DB2CA1839A431994AE826329A
          CD9498325AD398E810364776309629AC01F53341AC3D27B229FCDF88C232D7B0
          03551D7B0EEFB7424BF73436243B065E6AF890D00740A927E7E0DECABCD05B0B
          6C5E864CE187C814EE2CF45BD7E5C7A1E1DF50BC6C7F39CA7F41ACF802BB5E6C
          69CFBE376268E10EA35A79F9AD25DAB4091BD7A99A3A047E4E425D35E27D4B08
          5E74002391A94C4206213A84BA72760228BF532CD18CF7C7D0E13F6C84F83869
          B920A10F80B73F58A84C1E3BE1D162F3BE15F0AF3D3F465FB6E1E70B97ACD447
          6DD4F0BDCE6CFAB723870DED31659065BB630D5D13B7E2A6BA950771FB0F23EA
          DDD799B3E635D545339A548A35ADE964635DA2DEE37CAAAEA9C3106CA7290A1B
          874DE351776395759D8019A4135877EAA2EBDC4527EAB7053F7903EF0BF0E9F3
          B6EBBE9A308D3E17368C1224F401D2D2DA996A6A483D09B1EF10C03F3767395B
          26E2C67B1B7EBEF08395CAC4F11B159D130C0DEC1FEB9F42CBAF76FA3CCABA3E
          9373E6D6A7CCD618D127994CAE4ED5B5118A1F1BAF69AA3807301C39F7C6A84D
          718E403CC22BAAD64617D086BAFE08DBDA9145B5224378B7AD2DD33A6C587D9F
          936C4619127A0874A4734352095388BDD7E582857FAEEBCDD4F5E0FEE52790BC
          75DDFCECFEBD1E8F5DA96BECD972F9F2F18A9678635D3C6EEABA8A7F189930D1
          D1C4311C1027CC545F2C2DEDFB62DE4A0E51B878B754554126A1B8DC75D7304D
          AB64D5130121A187443A6B0D473AF777887D5A6FDF43A4D81D51E2E92065AE58
          D5DA387C68C38710D37CCFF57E8A0E22F07AE58558F0DE326DC2B811439598BF
          19434A0B813641C45B203F1DA52A4A1C5DC9701C82613125560F111BF9F5E245
          9E2BDE45D463DD8A1451503C9021AE112FB75C776B53D3D2B28E01511C127A88
          203D1C865828E66A9F5EEC3B88E86241852B8394F7C9F2969D9A1B5393717C02
          AD81DD1DDB76C63095CD8460B7C26B664C8C5563B13138EC0921DF904F57ADCA
          B9EEB8B8A655FCF64EA26F48E821D399C90D49C6CD0721F69D0B6D4704BD14DB
          CAB2D639F6DD10378D2F8A67DA1525268EA938236D54C8F5E510FA24127A3421
          A1978196B674AAA921790F04B74FF76D10BA48EFF70A6B5F8B96AE5637DBA479
          0F8CA50F83B8F791B85E1A093DC290D0CBC4F2D60E754443DD1F20EACF5D6787
          8F1FB577662734D627077CEF33E7FC708CAFC55A695364FB1B23A1471A127A99
          F13C7EBEAAB27336F091639CBEA5AE6BEF8450FC73787D49B68F7948E8118684
          5E0120F6A311D9FF80B43A9EFFFB4888FF8E108A7E18AFFD64FB9787841E6148
          E81502517C7BB1A022C43E11E3F4AB21FCD3422896844E0482845E413A3B73CD
          C9A4792BFE3B0D23ECB18CA9039D1490844E0482842E0144F76FA673D6238D75
          C9818A82844E0482845EDD90D0894090D0AB1B123A1108127A75434227024142
          AF6E48E8442048E8D50D099D080409BDBA21A1138120A157372474221024F4EA
          86844E0482845EDD90D0894090D0AB1B123A1108127A75434227024142AF6E48
          E8442048E8D50D099D080409BDBA21A1138120A157372474221024F4EA86844E
          0482845EDD90D0894090D0AB1B123A1108127A75434227024142AF6E48E84420
          48E8D50D099D080409BDBA21A1138120A157372474221024F4EA86844E048284
          5EDD90D0894090D0AB1B123A1188C8093D67B9931371FD3DD9B65409F7E375A0
          6C23F290D0234CE484CEB93F4755D9BBB26DA9127E87D72EB28DC843428F3091
          123A51D590D0230C099D080B127A8421A1136141428F302474222C48E8118684
          4E8405093DC290D089B020A14718123A111624F408434227C282841E6148E844
          5890D0230C099D080B127A84513A33B96753097357D9861055CFA736846E685A
          46B621444F9474C67A2C9930F6966D0851F52C716D7BBC6618BE6C43889E2839
          CBB9C934B463641B425437BEEF7FA028CA04D976108551B8C72F549872B66C43
          88EA86737F3E63CA4CD9761085516CC73D4D53D54B152566C83686A85E10D16F
          47443F4AB61D446144EA3EC5D0B52721F451B28D21AA17CFE367AB2ABB48B61D
          446194879F5AC0F69935E519A45D5F926D0C51BDB89EB71B32C36765DB411446
          5937AB0BBF983176966C6388EA046D6855266B8F4B254DBA861E51BA84EEBADE
          AE2A7A63A4EF0451329CFB7F414678B86C3B88E274097D6D7B466FAA4FBCA328
          CA78D90611D58608147CB6A6A98FC8B684284E97D0059EC7CF5455F66BD90611
          D505DACF7B9FB6B44CDD64E8502EDB16A238FF157A266B3525E2E622A4EF4365
          1B45540F0810C72140DC28DB0EA277FE2B74010EDA8F71D02E916D14511D8868
          BE724DDB1623863579B26D217AE773426FEB4CC7EB53C93719DDCA4804C0753D
          1A9B57099F13BAC071BDDD744D7D5AB66144B4E1DCBF8D31658E6C3B8860F410
          BAC0E3FC7C95B173641B474413EEFB8B2CCB9991881B1DB26D21825150E802F4
          D873D163EF2FDB40227274D8B6BB9361680B641B4204A7A8D0D7ACED4C3437A6
          1EA75B6389F5A0A920D9F366ABAAFAB86C5B88D2282A744167365B9734CD8718
          63B3641B4AC845AC680D957F5D53D943B26D214AA757A10B56AE698F0F6BAEBF
          1591FD10D9C61272401BF9D4F3F8619AA63E2FDB16A27FF429F4F5E0409FC554
          76A1128BA9B28D262A07E7FE93AEEB1D8B31F947B26D21FA4F60A10B6CD7DB51
          57D9EF1445D951B6E1447941BB6887C8CF5371BC65DB420C9C9284DEF5034511
          374A7C07A9FC0FE92198DA03CDC1F17D7E8BE3F18B4D5D5B2CDB1E221C4A16FA
          7A56AEED480C69481DC114E558687F67859E71AD6AD00E9672DFBF1B51FC165D
          53E9D2598DD16FA16F88EDB85BAA8CED8E28BF0BFEDC0AA21F86629BA07D5DB6
          83C4E711F38CE04D4C10B1167FBD8FE33F9FFBB1BFA7B3D63F1BEB1239D9F611
          E52114A16F486B47A6391937262B31654755638D88F83A22C548B12FD9CE0E62
          44BED5810EB8C57539B272FE6ECC8FBD689ADA32D9861195E1FF01FC545B56F0
          220BB40000000049454E44AE426082}
        Proportional = True
      end
      object lblVersion: TLabel
        Left = 660
        Top = 49
        Width = 80
        Height = 11
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'lblVersion'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
    end
    object WizardMapNodes: TJvWizardRouteMapNodes
      Left = 0
      Top = 65
      Width = 177
      Height = 394
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      AllowClickableNodes = False
      Color = 11103753
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.Layout = ilCenter
      NodeColors.Selected = clGreen
      NodeColors.Line = clBtnFace
    end
  end
  object JvCreateProcess: TJvCreateProcess
    Priority = ppHigh
    ConsoleOptions = [coOwnerData, coRedirect, coSeparateError]
    Left = 56
    Top = 216
  end
end

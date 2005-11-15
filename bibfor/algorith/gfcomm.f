      SUBROUTINE GFCOMM ( IT,Z,DZ,D2Z,DT, FPMEC, FFMEC, CARTER, CFPDC2,
     &                    FLUID, GAINE, GRAPE2, MANCHE, MECA1, RUGOSI,
     &                    TIGE, GRAPPE )
      IMPLICIT NONE
      INTEGER   IT
      REAL*8    CARTER(14), CFPDC2(5), FLUID(8), GAINE(7), GRAPE2(8),
     &          MANCHE(21), MECA1(17), RUGOSI(8), TIGE(5), GRAPPE(4),
     &          FPMEC, FFMEC, Z, DZ, D2Z, DT
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/11/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C    
C     CALCUL DE LA FORCE FLUIDE EXERCEE SUR LA TIGE DE COMMANDE
C     DANS LA MECANISME DE COMMANDE
C
C-----------------------------------------------------------------------
      INTEGER  IND, NMAX, I, IMANCH
      REAL*8   P2,CDM,CDA,CDML,CDI,CDG
      REAL*8   G,DHM,DHA,ROM,P3,P4,S
      REAL*8   LI,LML,DEML,DIML,DCSP,ROML,NUML,AML,AI,DHML,DHI
      REAL*8   LM,LA,LIM,DIMT,DEMT,ROMT,VMT,NUM,AM,AA,FR,AMT,ACMT
      REAL*8   LG,DG,ROG,NUG,AG,DHG,AT,LAMEQM,CFM,PI,UM,UA,D,MAMT
      REAL*8   LAMA,KM,KA,PM,PA,CF1,CF2,KCF1,KCF2,GFCORR,CFG,CFI
      REAL*8   DTIGE,UMM1,UAM1,NUA
      REAL*8   LMEQM1,DA,CFA,EPSLON,C1,C2,C3,LAMA1
      REAL*8   UI,UML,LAMEQI,LAMML,C4,C5,C6,KML,KI,UMLM1,UIM1
      REAL*8   LMEQI1,LAMML1,UG,LAMEQG,PS,PML
      REAL*8   L4,L5,LCT,LLT,LCM,LCI,LCG,CFCM,CFCI,CFCG
      REAL*8   HRUGC,HRUGTC,HRUGM,HRUGA,HRUGML,HRUGG,HRUGSP
      REAL*8   ROI,NUI,ROA,UN,ZERO, R8PI, DUM, DUI
      REAL*8   F(9)
C     ------------------------------------------------------------------
C
      LI   = CARTER(1)
      LML  = CARTER(2)
      DEML = CARTER(4)
      DIML = CARTER(5)
      DCSP = CARTER(6)
      ROML = CARTER(7)
      NUML = CARTER(8)
      AML  = CARTER(9)
      AI   = CARTER(10)
      DHML = CARTER(11)
      DHI  = CARTER(12)
      ROI  = CARTER(13)
      NUI  = CARTER(14)
C
      CDM  = CFPDC2(1)
      CDA  = CFPDC2(2)
      CDML = CFPDC2(3)
      CDI  = CFPDC2(4)
      CDG  = CFPDC2(5)
C
      P2 = FLUID(7)
      
      LG  = GAINE(1)
      DG  = GAINE(2)
      ROG = GAINE(3)
      NUG = GAINE(4)
      AG  = GAINE(5)
      DHG = GAINE(6)
      AT  = GAINE(7)
C      
      DTIGE = GRAPE2(1)
C      
      LM   = MANCHE(1)
      LA   = MANCHE(2)
      LIM  = MANCHE(3)
      DIMT = MANCHE(4)
      DEMT = MANCHE(5)
      ROMT = MANCHE(7)
      VMT  = MANCHE(8)
      NUM  = MANCHE(9)
      AM   = MANCHE(10)
      AA   = MANCHE(11)
      DHM  = MANCHE(12)
      DHA  = MANCHE(13)
      ROM  = MANCHE(14)
      P3   = MANCHE(15)
      P4   = MANCHE(16)
      AMT  = MANCHE(17)
      ACMT = MANCHE(18)
      DA   = MANCHE(19)
      ROA  = MANCHE(20)
      NUA  = MANCHE(21)
C
      UM    = MECA1(1)
      UA    = MECA1(2)
      UML   = MECA1(3)
      UI    = MECA1(4)
      UG    = MECA1(5)
      UMM1  = MECA1(6)
      UAM1  = MECA1(7)
      UMLM1 = MECA1(8)
      UIM1  = MECA1(9)
      PM    = MECA1(10)
      PA    = MECA1(11)
      PML   = MECA1(12)
      PS    = MECA1(13)
      CFM   = MECA1(14)
      CFG   = MECA1(15)
      CFI   = MECA1(16)
      CFA   = MECA1(17)
C     
      HRUGC  = RUGOSI(1)
      HRUGTC = RUGOSI(2)
      HRUGM  = RUGOSI(4)
      HRUGA  = RUGOSI(5)
      HRUGML = RUGOSI(6)
      HRUGSP = RUGOSI(7)
      HRUGG  = RUGOSI(8)
C      
      LLT   =  TIGE(1)
      LCT   =  TIGE(2)
      CFCM  =  TIGE(3)
      CFCI  =  TIGE(4)
      CFCG  =  TIGE(5)
C
      G   = GRAPPE(3)
C
      IMANCH = 0
      NMAX   = 200
      EPSLON = 1.0D-6
      ZERO   = 0.0D0
      UN     = 1.0D0
      PI     = R8PI()
C
C     CALCUL DES LONGUEURS DE TIGE CANNELEE ET LISSE DANS CHAQUE ZONE
C
      L4  = LG+LML+LIM-LLT-LCT
      L5  = LG+LML-LLT-LCT
      LCM = MIN(LM,MAX(ZERO,Z-L4))
      LCI = MIN(LI,LI+Z-L5)
      LCG = MAX(ZERO,LG-LLT-Z)
C
C----------------------------------------------------------------------
C                 ZONE MANCHE THERMIQUE ADAPTATEUR
C----------------------------------------------------------------------
C
      UMM1 = UM
      UAM1 = UA
C
      IF (UM.EQ.ZERO) THEN
         LAMEQM = ZERO
         CFM = ZERO
      ELSE
         CALL GFCFRV ( (UM+DZ)*DHM/NUM, HRUGTC/DHM, CF1 )
         CALL GFCFRV (      UM*DHM/NUM,  HRUGM/DHM, CF2 )
         KCF1 = GFCORR ( (UM+DZ)*DHM/NUM )
         KCF2 = GFCORR (      UM*DHM/NUM )
         CFM = ((LM-LCM)*CF1+LCM*CFCM)/LM
         LAMEQM = 4*( KCF2*CF2/(UN+DTIGE/DIMT) + 
     &                KCF1*CFM/(UN+DIMT/DTIGE)*(UN+DZ/UM)**2)
      ENDIF
C
      IF (UA.EQ.ZERO) THEN
         LAMA = ZERO
         CFA = ZERO
      ELSE
         CALL GFCFRV ( UA*DHA/NUA, HRUGM/DHA, CFA )
         CALL GFCFRV ( UA*DHA/NUA, HRUGA/DHA, CF2 )
         KCF2 = GFCORR ( UA*DHA/NUA )
         LAMA = 4*KCF2*( CFA/(UN+DA/DEMT) + CF2/(UN+DEMT/DA) )
      ENDIF
C
      IND = 0
 10   CONTINUE
      IND = IND+1
C
      KM = ROM/2*AMT*(LAMEQM*LM/DHM+CDM)+PI/2*DIMT*LM*ROM*CFM
      KA = -ROA/2*ACMT*(LAMA*LA/DHA+CDA)+PI/2*DEMT*LA*ROA*CFA
C      
C     CALCUL DE LA FORCE RESULTANTE APPLIQUEE SUR LA MANCHETTE THERMIQUE
C     EN SUPPOSANT QU'ELLE REPOSE SUR l'ADAPTATEUR
C
      MAMT = (ROMT-(ROM+ROA)/2)*VMT
      FR = -MAMT*G + (P4-P3)*ACMT + KM*(AT/AM)**2*DZ**2
     &                            + ROM*LM*AT/AM*D2Z*AMT
C
      IF (FR.LE.ZERO) THEN
C        MANCHETTE NON SOULEVEE
C
         IMANCH = 0
         UA = ZERO
         UM = AT/AM*DZ
      ELSE
C      
C        MANCHETTE SOULEVEE
C
         IMANCH = 1
         C1 = KA+KM*(AA/AM)**2
         C2 = -(ROA*LA*ACMT+ROM*LM*AMT*AA/AM)/DT-2*KM*AT*AA/AM**2*DZ
         C3 = FR+(ROA*LA*ACMT+ROM*LM*AMT*AA/AM)*UAM1/DT
         IF ((DZ.NE.ZERO).AND.(C1.NE.ZERO)) THEN
           UA = (-C2-SQRT(C2**2-4.D0*C1*C3))/2.D0/C1
         ELSE
           UA = ZERO
         ENDIF
         UM = AT/AM*DZ-AA/AM*UA
      ENDIF
      IF (UA.EQ.ZERO) THEN
        DUM = ZERO
      ELSE
        DUM = AT/AM*D2Z - AA/AM*(UA-UAM1)/DT
      ENDIF
C
      CALL GFCFRV( (UM+DZ)*DHM/NUM, HRUGC/DHM, CF1 )
      CALL GFCFRV(      UM*DHM/NUM, HRUGM/DHM, CF2 )
      KCF1 = GFCORR ( (UM+DZ)*DHM/NUM )
      KCF2 = GFCORR (      UM*DHM/NUM )
      CFM = ((LM-LCM)*CF1+LCM*CFCM)/LM
C
      IF (UM.GT.ZERO) THEN
         LMEQM1 = 4*( KCF2*CF2/(UN+DTIGE/DIMT) +
     &                KCF1*CFM/(UN+DIMT/DTIGE)*(UN+DZ/UM)**2)
      ELSE
         LMEQM1 = ZERO
      ENDIF
C
      IF (UA.EQ.ZERO) THEN
         LAMA1 = ZERO
         CFA = ZERO
      ELSE
         CALL GFCFRV ( UA*DHA/NUA, HRUGM/DHA, CFA )
         CALL GFCFRV ( UA*DHA/NUA, HRUGA/DHA, CF2 )
         KCF2 = GFCORR ( UA*DHA/NUA )
         LAMA1 = 4*KCF2*( CFA/(UN+DA/DEMT) + CF2/(UN+DEMT/DA) )
      ENDIF
C
      IF (((ABS(LMEQM1-LAMEQM).GT.EPSLON).OR.
     &    (ABS(LAMA-LAMA1).GT.EPSLON)).AND.(IND.LT.NMAX)) THEN
         LAMEQM = LMEQM1
         LAMA   = LAMA1
         GOTO 10
      ELSE
         IF (IND.GE.NMAX) THEN
            CALL UTDEBM('A','MANCHE THERMIQUE ADAPTATEUR',
     &                      'LAMEQM OU LAMA N''ONT PAS CONVERGE')
            CALL UTIMPI('S',' POUR L''ITERATION ', 1, IT)
            CALL UTFINM()
         ENDIF
      ENDIF 
C
      PM = P3-ROM/2*UM**2*(LAMEQM*LM/DHM+CDM)-ROM*LM*DUM
      PA = P4-ROA/2*UA**2*(LAMA*LA/DHA+CDA)-ROA*LA*(UA-UAM1)/DT
C
C----------------------------------------------------------------------
C                    ZONE CARTER/MECANISME DE LEVEE
C----------------------------------------------------------------------
C
      UIM1 = UI
      UMLM1 = UML
C
      IF (UI.EQ.ZERO) THEN
         LAMEQI = ZERO
      ELSE
         CALL GFCFRV ((UI+DZ)*DHI/NUI,  HRUGC/DHI, CF1 )
         CALL GFCFRV (     UI*DHI/NUI, HRUGML/DHI, CF2 )
         KCF1 = GFCORR ( (UI+DZ)*DHI/NUI )
         KCF2 = GFCORR (      UI*DHI/NUI )
         CFI  = ((LI-LCI)*CF1+LCI*CFCI)/LI
         LAMEQI = 4*( KCF2*CF2/(UN+DTIGE/DIML)+
     &                KCF1*CFI/(UN+DIML/DTIGE)*(UN+DZ/UI)**2 )
      ENDIF
C
      IF (UML.EQ.ZERO) THEN
         LAMML = ZERO
      ELSE
         CALL GFCFRV ( UML*DHML/NUML, HRUGML/DHML, CF1 )
         CALL GFCFRV ( UML*DHML/NUML, HRUGSP/DHML, CF2 )
         KCF2 = GFCORR ( UML*DHML/NUML )
         LAMML = 4*KCF2*( CF1/(UN+DCSP/DEML) + CF2/(UN+DEML/DCSP) )
      ENDIF
C
      IND = 0
 15   CONTINUE
      IND = IND+1
C
      KI = LAMEQI*LI/DHI+CDI
      KML = LAMML*LML/DHML+CDML
C
      C4 = ROML*KML-ROI*KI*(AML/AI)**2
      C5 = ROI*KI*AT*AML/AI**2*DZ+(ROML*LML+ROI*LI*AML/AI)/DT
      C6 = -((ROI*KI*(AT/AI)**2*DZ**2 + 2*ROI*LI*AT/AI*D2Z
     &     + 2*(ROML*LML+ROI*LI*AML/AI)*UMLM1/DT))
      IF ((DZ.NE.ZERO).AND.(C4.NE.ZERO)) THEN
         UML = (-C5+SQRT(C5**2-C4*C6))/C4
      ELSE
         UML = ZERO
      ENDIF
      UI = AT/AI*DZ - AML/AI*UML
      IF (UML.EQ.ZERO) THEN
        DUI = ZERO
      ELSE
        DUI = AT/AI*D2Z - AML/AI*(UML-UMLM1)/DT
      ENDIF
C
      IF (UI.EQ.ZERO) THEN
         LMEQI1 = ZERO
      ELSE
         CALL GFCFRV ( (UI+DZ)*DHI/NUI,  HRUGC/DHI, CF1 )
         CALL GFCFRV (      UI*DHI/NUI, HRUGML/DHI, CF2 )
         KCF1 = GFCORR ( (UI+DZ)*DHI/NUI )
         KCF2 = GFCORR (      UI*DHI/NUI )
         CFI  = ((LI-LCI)*CF1+LCI*CFCI)/LI
         LMEQI1 = 4*( KCF2*CF2/(UN+DTIGE/DIML) + 
     &                KCF1*CFI/(UN+DIML/DTIGE)*(UN+DZ/UI)**2 )
      ENDIF
C
      IF (UML.EQ.ZERO) THEN
         LAMML1 = ZERO
      ELSE
         CALL GFCFRV ( UML*DHML/NUML, HRUGML/DHML, CF1 )
         CALL GFCFRV ( UML*DHML/NUML, HRUGSP/DHML, CF2 )
         KCF2 = GFCORR ( UML*DHML/NUML )
         LAMML1 = 4*KCF2*( CF1/(UN+DCSP/DEML) + CF2/(UN+DEML/DCSP) )
      ENDIF
C
      IF ( ((ABS(LAMEQI-LMEQI1).GT.EPSLON).OR.
     &      (ABS(LAMML -LAMML1).GT.EPSLON)).AND.(IND.LT.NMAX)) THEN
         LAMML = LAMML1
         LAMEQI = LMEQI1
         GOTO 15
      ELSE
        IF (IND.GE.NMAX) THEN
            CALL UTDEBM('A','CARTER/MECANISME DE LEVEE',
     &                      'LAMEQI OU LAMML N''ONT PAS CONVERGE')
            CALL UTIMPI('S',' POUR L''ITERATION ', 1, IT)
            CALL UTFINM()
         ENDIF
      ENDIF
C
      PML = PM-ROI/2*UI**2*KI-ROI*LI*DUI
C
C----------------------------------------------------------------------
C                           ZONE GAINE DE TIGE
C----------------------------------------------------------------------
C
      UG = AT/AG*DZ
C
      CALL GFCFRV ( (UG+DZ)*DHG/NUG, HRUGC/DHG, CF1 )
      CALL GFCFRV (      UG*DHG/NUG, HRUGG/DHG, CF2 )
      KCF1 = GFCORR ( (UG+DZ)*DHG/NUG )
      KCF2 = GFCORR (      UG*DHG/NUG )
      CFG = ((LG-Z-LCG)*CF1+LCG*CFCG)/(LG-Z)
      LAMEQG = 4*( KCF2*CF2/(UN+DTIGE/DG) +
     &             KCF1*CFG/(UN+DG/DTIGE)*(UN+AG/AT)**2 )
C
      PS = PML-ROG/2*DZ**2*(UN+(AT/AG)**2*(LAMEQG*(LG-Z)/DHG+CDG-1))
     &      -ROG*(LG-Z)*AT/AG*D2Z

C
      MECA1(1) = UM
      MECA1(2) = UA
      MECA1(3) = UML
      MECA1(4) = UI
      MECA1(5) = UG
      MECA1(6) = UMM1
      MECA1(7) = UAM1
      MECA1(8) = UMLM1
      MECA1(9) = UIM1
      MECA1(10) = PM
      MECA1(11) = PA
      MECA1(12) = PML
      MECA1(13) = PS
      MECA1(14) = CFM
      MECA1(15) = CFG
      MECA1(16) = CFI
      MECA1(17) = CFA
C
C----------------------------------------------------------------------
C        CALCUL DE L'ERREUR RESIDUELLE DANS LE RESOLUTION DU SYSTEME
C----------------------------------------------------------------------
C
      F(1) = AT*DZ - AM*UM - AA*UA
C
      F(2) = PM - P3 + ROM/2*UM**2*(LAMEQM*LM/DHM+CDM)
     +               + ROM*LM*DUM
C
      F(3) = PA - P4 + ROA/2*UA**2*(LAMA*LA/DHA+CDA)
     +               + ROA*LA*(UA-UAM1)/DT
C
      IF ( IMANCH .EQ. 1 ) THEN
         F(4) = -MAMT*G + (P3-PM)*AMT + (PA-P3)*ACMT
     +                                + PI*DIMT/2*LM*ROM*CFM*UM**2
     &                                + PI*DEMT/2*LA*ROA*CFA*UA**2
      ELSE
         F(4) = 0.D0
      ENDIF
C
      F(5) = AT*DZ - AML*UML - AI*UI
C
      F(6) = PML - PM + ROI/2*UI**2*(LAMEQI*LI/DHI+CDI)
     +                + ROI*LI*DUI
C
      F(7) = PML - PM + ROML/2*UML**2*(LAMML*LML/DHML+CDML)
     +                + ROML*LML*(UML-UMLM1)/DT
C
      F(8) = AT*DZ - AG*UG
C
      F(9) = PS - PML + ROG/2*UG**2*(LAMEQG*(LG-Z)/DHG+CDG-1)
     +                + ROG/2*DZ**2 + ROG*(LG-Z)*AT/AG*D2Z
C
      DO 20 I = 1 , 9
        IF ( ABS(F(I)) .GT. 1.0D-3 )  THEN
          CALL UTDEBM('A','MECANISME','CALCUL DE L''ERREUR RESIDUELLE'//
     +    ' DANS LA RESOLUTION DU MODELE DANS LE MECANISME DE COMMANDE')
          CALL UTIMPR('L',' ABS(F) > 1.0D-3 , F = ', 1, F(I) )
          CALL UTFINM
        ENDIF
 20   CONTINUE
C
C----------------------------------------------------------------------
C                 CALCUL DE LA FORCE FLUIDE RESULTANTE
C----------------------------------------------------------------------
C
C --- FORCE DE PRESSION 
C
      FPMEC = (P2-PS)*AT
C
C --- FORCE DE FROTTEMENT VISQUEUX
C
      FFMEC = PI*DTIGE/2*(ROM*LM*CFM*(UM+DZ)**2+ROI*LI*CFI*(UI+DZ)**2
     &               + ROG*(LG-Z)*CFG*(UG+DZ)**2)
C
      END

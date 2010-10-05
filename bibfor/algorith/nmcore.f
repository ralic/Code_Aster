      SUBROUTINE NMCORE(SDIMPR,SDCRIT,NUMINS,PARCRI,VRESI ,
     &                  VRELA ,VMAXI ,VCHAR ,VREFE ,VNODA,CVNEWT,
     &                  MAXREL,MAXNOD)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/10/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      LOGICAL      CVNEWT,MAXREL,MAXNOD
      REAL*8       PARCRI(*)
      INTEGER      NUMINS
      CHARACTER*24 SDIMPR
      CHARACTER*19 SDCRIT
      REAL*8       VRESI,VRELA,VMAXI,VCHAR,VREFE,VNODA
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - CONVERGENCE)
C
C VERIFICATION DES CRITERES D'ARRET SUR RESIDUS
C
C ----------------------------------------------------------------------
C
C
C IN  VRESI  : RESIDU D'EQUILIBRE
C IN  VRELA  : RESI_GLOB_RELA MAXI
C IN  VMAXI  : RESI_GLOB_MAXI MAXI
C IN  VNODA  : RESI_COMP_RELA MAXI 
C IN  VCHAR  : CHARGEMENT EXTERIEUR MAXI
C IN  VREFE  : RESI_GLOB_REFE MAXI
C IN  SDIMPR : SD AFFICHAGE
C IN  SDCRIT : SYNTHESE DES RESULTATS DE CONVERGENCE POUR ARCHIVAGE
C IN  NUMINS : NUMERO DU PAS DE TEMPS
C IN  PARCRI : CRITERES DE CONVERGENCE (VOIR NMDOCN)
C OUT CVNEWT : .TRUE. SI CRITERES ARRET NEWTON ATTEINTS
C OUT MAXREL : .TRUE. SI CRITERE RESI_GLOB_RELA ET CHARGEMENT = 0,
C                             ON UTILISE RESI_GLOB_MAXI
C OUT MAXNOD : .TRUE. SI CRITERE RESI_COMP_RELA ET IERA = 1,
C                             ON UTILISE RESI_GLOB_MAXI
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      NRESI
      PARAMETER    (NRESI=4)
C
      CHARACTER*24 IMPCNA,CRITPL,CRITCR
      INTEGER      JIMPCA,JCRP,JCRR
      REAL*8       R8PREM,VALR(2),DETECT
      REAL*8       CHMINI,RESIEQ,RESDEF,VRESI1
      INTEGER      PLATIT,IRESI
      REAL*8       PLATRE
      CHARACTER*16 TYPECV
      LOGICAL      CONVOK(NRESI)
      REAL*8       RESI(NRESI),RESID(NRESI)
      LOGICAL      LRELA,LMAXI,LREFE,LCMP
      INTEGER      IFM,NIV
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- ACCES OBJETS JEVEUX
C
      IMPCNA = SDIMPR(1:14)//'CONV.ACT'
      CRITPL = SDCRIT(1:19)//'.PLAT'
      CRITCR = SDCRIT(1:19)//'.CRTR'
      CALL JEVEUO(IMPCNA,'E',JIMPCA)
      CALL JEVEUO(CRITPL,'E',JCRP  )
      CALL JEVEUO(CRITCR,'L',JCRR  )
C
C --- INITIALISATIONS
C
      CVNEWT = .TRUE.
      MAXREL = .FALSE.
      MAXNOD = .FALSE.
      CHMINI = ZR(JCRR+6-1)
      RESIEQ = ZR(JCRR+7-1)
      IF (NINT(PARCRI(7)).EQ.0) THEN
        TYPECV = 'PIC'
      ELSEIF (NINT(PARCRI(7)).EQ.1) THEN
        TYPECV = 'PLATEAU' 
        PLATIT = NINT(PARCRI(8))  
        PLATRE = PARCRI(9)   
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF  
C
C --- COLONNES A TESTER POUR LE CRITERE D'ARRET
C
      LRELA    = ZL(JIMPCA-1+1)
      LMAXI    = ZL(JIMPCA-1+2)
      LREFE    = ZL(JIMPCA-1+3) 
      LCMP     = ZL(JIMPCA-1+4) 
      RESI(1)  = VRELA
      RESI(2)  = VMAXI
      RESI(3)  = VREFE
      RESI(4)  = VNODA
C      
      RESID(1) = PARCRI(2)
      RESID(2) = PARCRI(3)
      RESID(3) = PARCRI(6)      
      RESID(4) = PARCRI(12)     
C
C --- CRITERES D'ARRET - CAS DU PIC
C
      IF (TYPECV.EQ.'PIC') THEN
        DO 10 IRESI = 1,NRESI
          IF (ZL(JIMPCA+IRESI-1)) THEN
            CALL NMCORU(RESI(IRESI),RESID(IRESI),CONVOK(IRESI))
          ELSE
            CONVOK(IRESI) = .TRUE.  
          ENDIF  
  10    CONTINUE      
      ELSEIF (TYPECV.EQ.'PLATEAU') THEN     
        DO 15 IRESI = 1,NRESI
          IF (ZL(JIMPCA+IRESI-1)) THEN
            CALL NMCORT(SDCRIT,NRESI ,IRESI ,RESI(IRESI),RESID(IRESI),
     &                  PLATIT,PLATRE,CONVOK(IRESI))
          ELSE
            CONVOK(IRESI) = .TRUE. 
          ENDIF  
  15    CONTINUE        
      ELSE
        CALL ASSERT(.FALSE.)  
      ENDIF
C
C --- SI CRITERE RESI_GLOB_RELA ET CHARGEMENT = 0,
C --- ON UTILISE RESI_GLOB_MAXI
C 
      IF (LRELA) THEN
        DETECT  = 1.D-6 * CHMINI
        VALR(1) = DETECT
        VALR(2) = RESIEQ
        IF (VCHAR .LT. DETECT) THEN
          CALL U2MESR('I','MECANONLINE2_98',2,VALR  )
          IF (NUMINS.GT.1) THEN
            CONVOK(1) = .FALSE.
            IF ((VRESI.LT.RESIEQ).OR. 
     &          (VRESI.LT.R8PREM())) THEN
              CONVOK(1) = .TRUE.
              MAXREL    = .TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF          
C --- SI CRITERE RESI_COMP_RELA ET PREMIER INSTANT ,
C --- ON UTILISE RESI_GLOB_RELA SI CHARGEMENT NON NUL, RESI_GLOB_MAXI 
C 
C
      RESDEF =  RESID(4)
      IF (LCMP) THEN
          IF (NUMINS.EQ.1) THEN
            CONVOK(4) = .FALSE.
            IF(VNODA.LT.R8PREM())THEN
               IF(VCHAR .LE. (1.D-6 * CHMINI )) THEN
                   VRESI1=VMAXI
               ELSE
                   VRESI1=VRELA
               ENDIF 
            ELSE
                VRESI1=VNODA
            ENDIF
C            
            IF ((VRESI1.LT.RESDEF).OR. 
     &            (VRESI1.LT.R8PREM())) THEN
                 CONVOK(4) = .TRUE.
                 MAXNOD    = .TRUE.
            ELSE 
                 CONVOK(4) = .FALSE.
                 MAXNOD    = .FALSE.
            ENDIF
          ENDIF
      ENDIF   
             
C
C --- AFFICHAGES
C
      IF (LRELA) THEN
        IF (CONVOK(1)) THEN
          CALL IMPSDM(SDIMPR,'RESI_RELA',' ')
        ELSE
          CALL IMPSDM(SDIMPR,'RESI_RELA','X')
        ENDIF 
      ELSE
        CALL IMPSDM(SDIMPR,'RESI_RELA',' ') 
      ENDIF 
      
      IF (LMAXI) THEN 
        IF (CONVOK(2)) THEN
          CALL IMPSDM(SDIMPR,'RESI_MAXI',' ')
        ELSE
          CALL IMPSDM(SDIMPR,'RESI_MAXI','X')
        ENDIF
      ELSE
        CALL IMPSDM(SDIMPR,'RESI_MAXI',' ') 
      ENDIF    

      
      IF (LREFE) THEN       
        IF (CONVOK(3)) THEN
          CALL IMPSDM(SDIMPR,'RESI_REFE',' ')
        ELSE
          CALL IMPSDM(SDIMPR,'RESI_REFE','X')
        ENDIF 
      ELSE
        CALL IMPSDM(SDIMPR,'RESI_REFE',' ')
      ENDIF     
      IF (LCMP) THEN       
        IF (CONVOK(4)) THEN
          CALL IMPSDM(SDIMPR,'RESI_COMP',' ')
        ELSE
          CALL IMPSDM(SDIMPR,'RESI_COMP','X')
        ENDIF 
      ELSE
        CALL IMPSDM(SDIMPR,'RESI_COMP',' ')
      ENDIF     
C
C --- CONVERGENCE GLOBALE
C
      DO 20 IRESI = 1,NRESI
        CVNEWT = CVNEWT.AND.CONVOK(IRESI) 
  20  CONTINUE               
C
      CALL JEDEMA()
      END

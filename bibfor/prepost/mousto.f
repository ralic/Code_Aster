      SUBROUTINE MOUSTO ( GUIDAG, DIMTUB, VOLTUB, TUBUSE,  
     &                            DIMOBS, VOLOBS, OBSUSE, RCRAY, 
     &                    RCARTE, SECT, ARETE, ARETE2, NS, OBCONT, 
     &                    EPAIS, ECRAY, NOMT19, DENC, PERCE )
      IMPLICIT   NONE
      INTEGER             DIMTUB, DIMOBS, NS
      REAL*8              VOLOBS(*), OBSUSE(*), RCRAY, RCARTE, SECT(*),
     &                    ARETE, ARETE2, EPAIS, ECRAY, DENC, PERCE,
     &                    VOLTUB(*), TUBUSE(*)
      CHARACTER*8         OBCONT, GUIDAG
      CHARACTER*19        NOMT19
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C-----------------------------------------------------------------------
C     CALCULE LES VOLUMES USES DE L'OBSTACLE
C        GUIDAGE : ENCO_1 , ENCO_2 , CERCLE
C ----------------------------------------------------------------------
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------
      INTEGER      TYPUTU(20), TYPUOB(20), I
      INTEGER      NCO, IRAYO, ITHET, IREFE
      REAL*8       PARUTU(20,4), ADEBTU, AMAXTU, AFINTU, PROFTU
      REAL*8       PARUOB(20,4), ADEBOB, AMAXOB, AFINOB, PROFOB
      REAL*8       PROFON, ANSINI, ANSFIN, ANGARE, ANGVA
      REAL*8       VOLUME, RAPT, RAPO, R8PREM
      CHARACTER*8  K8B
      CHARACTER*16 CONCEP, NOMCMD
C-----------------------------------------------------------------------
C
C     CALCUL DES PARAMETRES DES USURES :
C     --------------------------------
C
      DO 130 I = 1 , NS
C
         ANSINI = SECT(I) 
         ANSFIN = SECT(I+1) 
C
         TYPUTU(I) = 0
         ADEBTU = 0.D0
         AMAXTU = 0.D0
         AFINTU = 0.D0
         PROFTU = 0.D0
C
         TYPUOB(I) = 0
         ADEBOB = 0.D0
         AMAXOB = 0.D0
         AFINOB = 0.D0
         PROFOB = 0.D0
C
         IF ( GUIDAG .EQ. 'CERCLE' ) THEN
C             --------------------
C --------- USURE EN LUNULE
            VOLUME = VOLTUB(I) + VOLOBS(I)
            IF ( VOLUME .GT. R8PREM() ) THEN
               CALL LUNULE ( RCRAY, RCARTE, ADEBTU, AFINTU, AMAXTU, 
     &                       ANSINI, ANSFIN, PROFON, VOLUME, EPAIS )
               RAPT = VOLTUB(I) / VOLUME
               RAPO = VOLOBS(I) / VOLUME
               TYPUTU(I) = 1
               PROFTU = PROFON * RAPT
               TYPUOB(I) = 1
               ADEBOB = ADEBTU
               AMAXOB = AMAXTU
               AFINOB = AFINTU
               PROFOB = PROFON * RAPO
            ENDIF
C
         ELSEIF ( GUIDAG .EQ. 'ENCO_1' ) THEN
C                 --------------------
            IF ( ( NS.EQ.12 .AND. (I.EQ.1 .OR. I.EQ.12) ) .OR.  
     &           ( NS.EQ.10 .AND. (I.EQ.1 .OR. I.EQ.10) ) ) THEN
C ------------ USURE EN VE
               IF ( I .EQ. 1 ) THEN
                  ANGARE = ARETE
               ELSE
                  ANGARE = 360.D0 - ARETE
               ENDIF
               ANGVA  = 28.D0
               VOLUME = VOLTUB(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VETUBE ( RCRAY, RCARTE, ADEBTU, AFINTU, ANGARE, 
     &                          AMAXTU, ANGVA, PROFTU, VOLUME, EPAIS )
                  TYPUTU(I) = 3
               ENDIF
               VOLUME = VOLOBS(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL MOUVEO ( ARETE, RCARTE, ADEBOB, AFINOB, 
     &                          ANGARE, AMAXOB, PROFOB, VOLUME, EPAIS )
                  TYPUOB(I) = 3
               ENDIF
            ELSEIF ( ( NS.EQ.12 .AND. (I.EQ.2 .OR. I.EQ.11) ) .OR.  
     &               ( NS.EQ.10 .AND. (I.EQ.2 .OR. I.EQ.9 ) ) ) THEN
C ------------ USURE EN VE+LUNULE
               IF ( I .EQ. 2 ) THEN
                  ANGARE = ARETE
               ELSE
                  ANGARE = 360.D0 - ARETE
               ENDIF
               ANGVA  = 11.D0
               VOLUME = VOLTUB(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VETUBE ( RCRAY, RCARTE, ADEBTU, AFINTU, ANGARE, 
     &                          AMAXTU, ANGVA, PROFTU, VOLUME, EPAIS )
                  TYPUTU(I) = 2
               ENDIF
               VOLUME = VOLOBS(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VEOBST ( ARETE, RCARTE, ADEBOB, AFINOB, ANGVA, 
     &                          ANGARE, AMAXOB, PROFOB, VOLUME, EPAIS )
                  TYPUOB(I) = 2
               ENDIF
            ELSE
C ------------ USURE EN LUNULE
               VOLUME = VOLTUB(I) + VOLOBS(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL LUNULE ( RCRAY, RCARTE, ADEBTU, AFINTU, AMAXTU, 
     &                          ANSINI, ANSFIN, PROFON, VOLUME, EPAIS )
                  RAPT = VOLTUB(I) / VOLUME
                  RAPO = VOLOBS(I) / VOLUME
                  TYPUTU(I) = 1
                  PROFTU = PROFON * RAPT
                  TYPUOB(I) = 1
                  ADEBOB = ADEBTU
                  AMAXOB = AMAXTU
                  AFINOB = AFINTU
                  PROFOB = PROFON * RAPO
               ENDIF
               IF ( VOLTUB(I) .GT. R8PREM() ) THEN
                  CALL LUNULE ( RCRAY, RCARTE, ADEBTU, AFINTU, AMAXTU, 
     &                       ANSINI, ANSFIN, PROFON, VOLTUB(I), EPAIS )
                  PROFTU = PROFON
               ENDIF
               IF ( VOLOBS(I) .GT. R8PREM() ) THEN
                  CALL LUNULE ( RCRAY, RCARTE, ADEBTU, AFINTU, AMAXTU, 
     &                       ANSINI, ANSFIN, PROFON, VOLOBS(I), EPAIS )
                  PROFOB = PROFON 
               ENDIF
            ENDIF
C
         ELSEIF ( GUIDAG .EQ. 'ENCO_2' ) THEN
C                 --------------------
            IF ( ( NS.EQ.12 .AND. (I.EQ.1 .OR. I.EQ.12) ) .OR.  
     &           ( NS.EQ.10 .AND. (I.EQ.1 .OR. I.EQ.10) ) ) THEN
C ------------ USURE EN VE
               IF ( I .EQ. 1 ) THEN
                  ANGARE = ARETE
               ELSE
                  ANGARE = 360.D0 - ARETE
               ENDIF
               ANGVA  = 28.D0
               VOLUME = VOLTUB(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VETUBE ( RCRAY, RCARTE, ADEBTU, AFINTU, ANGARE, 
     &                          AMAXTU, ANGVA, PROFTU, VOLUME, EPAIS )
                  TYPUTU(I) = 3
               ENDIF
               VOLUME = VOLOBS(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL MOUVEO ( ARETE, RCARTE, ADEBOB, AFINOB, 
     &                          ANGARE, AMAXOB, PROFOB, VOLUME, EPAIS )
                  TYPUOB(I) = 3
               ENDIF
            ELSEIF ( ( NS.EQ.12 .AND. (I.EQ.6 .OR. I.EQ.7) ) .OR.  
     &               ( NS.EQ.10 .AND. (I.EQ.5 .OR. I.EQ.6) ) ) THEN
C ------------ USURE EN VE 2E ENCOCHE
               IF ( NS.EQ.12 .AND. I.EQ.6 ) THEN
                  ANGARE = ARETE2
               ELSEIF ( NS.EQ.10 .AND. I.EQ.5 ) THEN
                  ANGARE = ARETE2
               ELSE
                  ANGARE = 360.D0 - ARETE2
               ENDIF
               ANGVA  = 28.D0
               VOLUME = VOLTUB(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VETUBE ( RCRAY, RCARTE, ADEBTU, AFINTU, ANGARE, 
     &                          AMAXTU, ANGVA, PROFTU, VOLUME, EPAIS )
                  TYPUTU(I) = 5
               ENDIF
               VOLUME = VOLOBS(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL MOUVEO ( ARETE, RCARTE, ADEBOB, AFINOB, 
     &                          ANGARE, AMAXOB, PROFOB, VOLUME, EPAIS )
                  TYPUOB(I) = 5
               ENDIF
            ELSEIF ( ( NS.EQ.12 .AND. (I.EQ.2 .OR. I.EQ.11) ) .OR.  
     &               ( NS.EQ.10 .AND. (I.EQ.2 .OR. I.EQ.9 ) ) ) THEN
C ------------ USURE EN VE+LUNULE
               IF ( I .EQ. 2 ) THEN
                  ANGARE = ARETE
               ELSE
                  ANGARE = 360.D0 - ARETE
               ENDIF
               ANGVA  = 11.D0
               VOLUME = VOLTUB(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VETUBE ( RCRAY, RCARTE, ADEBTU, AFINTU, ANGARE, 
     &                          AMAXTU, ANGVA, PROFTU, VOLUME, EPAIS )
                  TYPUTU(I) = 2
               ENDIF
               VOLUME = VOLOBS(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VEOBST ( ARETE, RCARTE, ADEBOB, AFINOB, ANGVA, 
     &                          ANGARE, AMAXOB, PROFOB, VOLUME, EPAIS )
                  TYPUOB(I) = 2
               ENDIF
            ELSEIF ( ( NS.EQ.12 .AND. (I.EQ.5 .OR. I.EQ.8) ) .OR.  
     &               ( NS.EQ.10 .AND. (I.EQ.4 .OR. I.EQ.7) ) ) THEN
C ------------ USURE EN VE+LUNULE 2E ENCOCHE
               IF ( I .EQ. 4  .OR.  I .EQ. 5 ) THEN
                  ANGARE = ARETE2
               ELSE
                  ANGARE = 360.D0 - ARETE2
               ENDIF
               ANGVA  = 11.D0
               VOLUME = VOLTUB(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VETUBE ( RCRAY, RCARTE, ADEBTU, AFINTU, ANGARE, 
     &                          AMAXTU, ANGVA, PROFTU, VOLUME, EPAIS )
                  TYPUTU(I) = 4
               ENDIF
               VOLUME = VOLOBS(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL VEOBST ( ARETE, RCARTE, ADEBOB, AFINOB, ANGVA, 
     &                          ANGARE, AMAXOB, PROFOB, VOLUME, EPAIS )
                  TYPUOB(I) = 4
               ENDIF
            ELSE
C ------------ USURE EN LUNULE
               VOLUME = VOLTUB(I) + VOLOBS(I)
               IF ( VOLUME .GT. R8PREM() ) THEN
                  CALL LUNULE ( RCRAY, RCARTE, ADEBTU, AFINTU, AMAXTU, 
     &                          ANSINI, ANSFIN, PROFON, VOLUME, EPAIS )
                  RAPT = VOLTUB(I) / VOLUME
                  RAPO = VOLOBS(I) / VOLUME
                  TYPUTU(I) = 1
                  PROFTU = PROFON * RAPT
                  TYPUOB(I) = 1
                  ADEBOB = ADEBTU
                  AMAXOB = AMAXTU
                  AFINOB = AFINTU
                  PROFOB = PROFON * RAPO
               ENDIF
            ENDIF
         ENDIF
C
         PARUTU(I,1) = ADEBTU
         PARUTU(I,2) = AMAXTU
         PARUTU(I,3) = AFINTU
         PARUTU(I,4) = PROFTU
C
         PARUOB(I,1) = ADEBOB
         PARUOB(I,2) = AMAXOB
         PARUOB(I,3) = AFINOB
         PARUOB(I,4) = PROFOB
C
 130  CONTINUE
C
C
C     CALCUL DE LA PROFONDEUR EN CUMULANT LES SECTEURS :
C     --------------------------------------------------
C
      IF ( GUIDAG .EQ. 'CERCLE' ) THEN
         CALL USOBCE ( DIMOBS, OBSUSE, RCARTE, NOMT19, NS,
     &                                 PARUOB, TYPUOB )
      ELSE
         CALL JEVEUO ( OBCONT//'           .VALR', 'L', IRAYO )
         CALL JEVEUO ( OBCONT//'           .VALT', 'L', ITHET )
         CALL JEVEUO ( OBCONT//'           .REFO', 'L', IREFE )
         CALL JELIRA ( OBCONT//'           .VALT', 'LONMAX', NCO, K8B )
         CALL USOBEN ( GUIDAG, DIMOBS, OBSUSE, NCO, ZR(IRAYO), 
     &                 ZR(ITHET), NS, PARUOB, TYPUOB, NOMT19,
     &                 ARETE, ARETE2, RCARTE, DENC )
      ENDIF
C
      CALL USTUEN ( DIMTUB, TUBUSE, RCRAY, NOMT19, NS, PARUTU, TYPUTU )
C
C
C     VERIFICATION DU NON PERCEMENT :
C     -----------------------------
C
      DO 200 I = 1 , DIMTUB
         PROFTU = ABS( TUBUSE(2*I) - RCRAY )
         IF ( PROFTU .GT. ECRAY*PERCE ) THEN
            CALL GETRES ( K8B, CONCEP, NOMCMD )
            CALL UTMESS('A',NOMCMD,'******* PERCEMENT TUBE *******')
            GOTO 202
         ENDIF 
C
 200  CONTINUE
 202  CONTINUE
C
      END

      SUBROUTINE SIGMCA(TABLCA,CARSIG,ICABL,NBNOCA,NUMACA)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 24/10/2000   AUTEUR CIBHHLV L.VIVAN 
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
C  DESCRIPTION : MISE A JOUR DE LA CARTE ELEMENTAIRE DES CONTRAINTES
C  -----------   INITIALES POUR LE CABLE COURANT
C                APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
C
C  IN     : TABLCA : CHARACTER*19
C                    NOM DE LA TABLE DECRIVANT LES CABLES
C  IN     : CARSIG : CHARACTER*19 , SCALAIRE
C                    NOM DE LA CARTE ELEMENTAIRE DES CONTRAINTES
C                    INITIALES
C  IN     : ICABL  : INTEGER , SCALAIRE
C                    NUMERO DU CABLE
C  IN     : NBNOCA : INTEGER , VECTEUR DE DIMENSION NBCABL
C                    CONTIENT LES NOMBRES DE NOEUDS DE CHAQUE CABLE
C  IN     : NUMACA : CHARACTER*19 , SCALAIRE
C                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
C                    NUMEROS DES MAILLES APPARTENANT AUX CABLES
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNOM, JEXNUM, JEXATR
C     ----- FIN   COMMUNS NORMALISES  JEVEUX  --------------------------
C
C ARGUMENTS
C ---------
      CHARACTER*19  CARSIG, NUMACA, TABLCA
      INTEGER       ICABL, NBNOCA(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER       IDECMA, IDECNO, IMAIL, IPARA, JNUMAC, JTBLP, JTBNP,
     &              JTENS, JVALV, NBLIGN, NBMACA, NBNO, NBPARA, NUMAIL
      CHARACTER*1   K1B
      CHARACTER*24  TENS
      LOGICAL       TROUVE
C
      CHARACTER*24  PARCR
      DATA          PARCR /'TENSION                 '/
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL JEMARQ()
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   ACCES AUX DONNEES ET AUX RESULTATS DE CALCUL UTILES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      NBNO = NBNOCA(ICABL)
C
C 1.1 RECUPERATION DE LA TENSION LE LONG DES CABLES
C ---
      CALL JEVEUO(TABLCA//'.TBNP','L',JTBNP)
      NBPARA = ZI(JTBNP)
      NBLIGN = ZI(JTBNP+1)
      CALL JEVEUO(TABLCA//'.TBLP','L',JTBLP)
      TROUVE = .FALSE.
      DO 10 IPARA = 1, NBPARA
         IF ( ZK24(JTBLP+4*(IPARA-1)).EQ.PARCR ) THEN
            TROUVE = .TRUE.
            TENS = ZK24(JTBLP+4*(IPARA-1)+2)
            CALL JEVEUO(TENS,'L',JTENS)
         ENDIF
         IF ( TROUVE ) GO TO 11
  10  CONTINUE
  11  CONTINUE
      IDECNO = NBLIGN - NBNO
C
C 1.2 NUMEROS DES MAILLES APPARTENANT AUX CABLES
C ---
      CALL JELIRA(NUMACA,'LONUTI',NBMACA,K1B)
      CALL JEVEUO(NUMACA,'L',JNUMAC)
      IDECMA = NBMACA - NBNO + 1
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   MISE A JOUR DE LA CARTE DES CONTRAINTES INITIALES AUX ELEMENTS
C     DES CABLES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C.... BOUCLE SUR LE NOMBRE DE MAILLES DU CABLE COURANT
C
      CALL JEVEUO(CARSIG//'.VALV','E',JVALV)
      DO 20 IMAIL = 1, NBNO-1
         NUMAIL = ZI(JNUMAC+IDECMA+IMAIL-1)
         ZR(JVALV) = ( ZR(JTENS+IDECNO+IMAIL-1)
     &               + ZR(JTENS+IDECNO+IMAIL) ) / 2.0D0
         CALL NOCART(CARSIG,3,K1B,'NUM',1,K1B,NUMAIL,' ',1)
  20  CONTINUE
C
      CALL JEDEMA()
C
C --- FIN DE SIGMCA.
      END

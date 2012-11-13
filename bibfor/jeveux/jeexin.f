      SUBROUTINE JEEXIN ( NOMLU , IRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 13/11/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      CHARACTER *(*)      NOMLU
      INTEGER                     IRET
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR : TESTE L'EXISTENCE DU DESCRIPTEUR CREE PAR
C                       JECREO OU JECROC
C IN  NOMLU  : NOM DE L'OBJET JEVEUX
C OUT IRET   : =0 LE DESCRIPTEUR N'EXISTE PAS
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IBACOL ,IBLONG ,IBNUM ,IC ,ICRE ,ID ,ILONG 
      INTEGER ITAB ,IXLONG ,IXNOM ,IXNUM ,JCARA ,JCTAB ,JDATE 
      INTEGER JHCOD ,JIADD ,JIADM ,JLONG ,JLONO ,JLTYP ,JLUTI 
      INTEGER JMARQ ,N ,NUTI 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C ----------------------------------------------------------------------
      INTEGER        IDNOM      , IDLONG     ,IDNUM
      PARAMETER    ( IDNOM  = 5 , IDLONG = 7 ,IDNUM  = 10 )
C ----------------------------------------------------------------------
      CHARACTER *32    NOML32
C DEB ------------------------------------------------------------------
      NOML32 = NOMLU
      IRET = 0
      ICRE = 0
      ID   = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
      IF ( IRET .EQ. 1 ) THEN
        IF ( NOML32(25:32) .EQ. '        ') THEN
           ID = IDATOS
        ELSE
           IF ( IADM(JIADM(ICLAOS)+2*IDATOS-1) .EQ. 0 ) THEN
             CALL JXVEUO ('L',ITAB,IRET,JCTAB)
           ENDIF
           CALL JJCROC ( '        ' , ICRE )
           ID = IDATOC
        ENDIF
      ELSE IF ( IRET .EQ. 2 ) THEN
        IC = ICLACO
        IF ( NOML32(25:32) .EQ. '        ') THEN
          ID = IDATCO
        ELSE
          CALL JJALLC (IC , IDATCO , 'L' , IBACOL )
          CALL JJCROC ( NOML32(25:32) , ICRE )
          IXNUM  = ISZON(JISZON+IBACOL+IDNUM )
          IXNOM  = ISZON(JISZON+IBACOL+IDNOM )
          IXLONG = ISZON(JISZON+IBACOL+IDLONG)
          IF ( IXNUM .NE. 0 ) THEN
            IBNUM = IADM(JIADM(IC)+2*IXNUM-1)
            NUTI  = ISZON(JISZON+IBNUM+1)
          ELSE
            NUTI  = LUTI ( JLUTI(IC) + IXNOM )
          ENDIF
          ID = IDATOC
          IF ( IXLONG .NE. 0 ) THEN
            IBLONG = IADM(JIADM(IC)+2*IXLONG-1)
            ILONG   = ISZON(JISZON+IBLONG - 1 + IDATOC)
            IF ( ILONG .LE. 0 ) ID = 0
          ELSE
            IF ( IDATOC .GT. NUTI ) ID = 0
          ENDIF
        ENDIF
      ENDIF
      IRET = ID
C FIN ------------------------------------------------------------------
      END

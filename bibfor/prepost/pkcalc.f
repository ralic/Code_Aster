      SUBROUTINE PKCALC ( NDIM, NBVAL, ABSSUP, DXSUP, DYSUP,  
     +                    DZSUP, ABSINF, DXINF, DYINF, DZINF, 
     +                    COEFD, COEFD3, COEFG, COEFG3,KG1,KG2,KG3)
      IMPLICIT   NONE
      INTEGER             NDIM,NBVAL
      REAL*8              COEFD,COEFD3,COEFG,COEFG3,KG1(*),KG2(*),KG3(*)
      CHARACTER*24        ABSSUP, DXSUP, DYSUP, DZSUP, 
     +                    ABSINF, DXINF, DYINF, DZINF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 17/08/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D WWW.CODE-ASTER.ORG
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
C
C     OPERATEUR POST_K1_K2_K3 :  CALCUL DES K1, K2, K3
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      IBID, I, IFM, NIV, IER, JABSCS, JABSCI, JDXS, JDYS,
     +             JDZS, JDXI, JDYI, JDZI, JSAUTX, JSAUTY, JSAUTZ,
     +             ISIGX, ISIGY, ISIGZ
      REAL*8       R8B, R, R1, R2, K1, K2, K3, G, RMAXEM, R8PREM,
     +             DXS, DYS, DZS, DXI, DYI, DZI, Y1, Y2, VK1, VK2, VK3,
     +             DE, RN
      CHARACTER*8  K8B
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ ( )
      CALL INFNIV ( IFM , NIV )
C
      CALL JEVEUO ( ABSSUP , 'L', JABSCS )
      CALL JEVEUO ( DXSUP  , 'L', JDXS   )
      CALL JEVEUO ( DYSUP  , 'L', JDYS   )
      CALL JEVEUO ( DZSUP  , 'L', JDZS   )
C
      CALL JEVEUO ( ABSINF , 'L', JABSCI )
      CALL JEVEUO ( DXINF  , 'L', JDXI   )
      CALL JEVEUO ( DYINF  , 'L', JDYI   )
      CALL JEVEUO ( DZINF  , 'L', JDZI   )
C
      CALL WKVECT ( '&&PKCALC.SAUT_DX' , 'V V R', NBVAL, JSAUTX )
      CALL WKVECT ( '&&PKCALC.SAUT_DY' , 'V V R', NBVAL, JSAUTY )
      IF ( NDIM .EQ. 3 ) THEN
         CALL WKVECT ( '&&PKCALC.SAUT_DZ' , 'V V R', NBVAL, JSAUTZ )
      ENDIF
C
      IF ( NIV .EQ. 2 )  THEN
         IF ( NDIM .EQ. 3 ) THEN
            WRITE(IFM,1001)
         ELSE
            WRITE(IFM,1000)
         ENDIF
      ENDIF
C
      ISIGX = 1
      ISIGY = 1
      ISIGZ = 1
      DO 30 I = 1 , NBVAL
         R   = ZR(JABSCS+I-1)
         DXS = ZR(JDXS+I-1)
         DYS = ZR(JDYS+I-1)
         DXI = ZR(JDXI+I-1)
         DYI = ZR(JDYI+I-1)
         IF ( I .EQ. NBVAL ) THEN
            R8B = DXS - DXI
            IF ( ABS(R8B).GT.R8PREM() .AND. R8B.LT.0.D0 ) ISIGX = -1
            R8B = DYS - DYI
            IF ( ABS(R8B).GT.R8PREM() .AND. R8B.LT.0.D0 ) ISIGY = -1
         ENDIF
         ZR(JSAUTX+I-1) = ( COEFD * ( DXS - DXI ) )**2
         ZR(JSAUTY+I-1) = ( COEFD * ( DYS - DYI ) )**2
         IF ( NDIM .EQ. 3 ) THEN
            DZS = ZR(JDZS+I-1)
            DZI = ZR(JDZI+I-1)
            ZR(JSAUTZ+I-1) = ( COEFD3 * ( DZS - DZI ) )**2
            IF ( I .EQ. NBVAL ) THEN
               R8B = DZS - DZI
               IF ( ABS(R8B).GT.R8PREM() .AND. R8B.LT.0.D0 ) ISIGZ = -1
            ENDIF
         ENDIF
         IF ( NIV .EQ. 2 ) THEN
            IF ( NDIM .EQ. 3 ) THEN
               WRITE(IFM,1011) R , DXS , DXI , ZR(JSAUTX+I-1) ,
     +                             DYS , DYI , ZR(JSAUTY+I-1) ,
     +                             DZS , DZI , ZR(JSAUTZ+I-1)
            ELSE
               WRITE(IFM,1010) R , DXS , DXI , ZR(JSAUTX+I-1) ,
     +                             DYS , DYI , ZR(JSAUTY+I-1)
            ENDIF
         ENDIF
 30   CONTINUE
C
C     ------------------------------------------------------------------
C                          --- METHODE 1 ---
C     ------------------------------------------------------------------
C
      IF ( NIV .EQ. 2 )  THEN
         IF ( NDIM. EQ .2 ) THEN
            WRITE(IFM,1020)
         ELSEIF( NDIM. EQ. 3) THEN
            WRITE(IFM,1021)
         ENDIF
      ENDIF
      DO 100 I = 1 , 8 , 2
         KG1(I) = -RMAXEM()
 100  CONTINUE
      DO 110 I = 2 , 8 , 2
         KG1(I) = RMAXEM()
 110  CONTINUE
      DO 102 I = 2 , NBVAL-1
         R1 = ZR(JABSCS+I-1)
         R2 = ZR(JABSCS+I-1+1)
         Y1 = ZR(JSAUTX+I-1) / R1
         Y2 = ZR(JSAUTX+I-1+1) / R2
         K1 = ABS( Y1 - R1 * ( Y2 - Y1 ) / ( R2 - R1 ) )
         Y1 = ZR(JSAUTY+I-1) / R1
         Y2 = ZR(JSAUTY+I-1+1) / R2
         K2 = ABS( Y1 - R1 * ( Y2 - Y1 ) / ( R2 - R1 ) )
         G  = COEFG * ( K2 + K1 )
         KG1(1) = MAX ( KG1(1) , K1 )
         KG1(2) = MIN ( KG1(2) , K1 )
         KG1(3) = MAX ( KG1(3) , K2 )
         KG1(4) = MIN ( KG1(4) , K2 )
         IF ( NDIM .EQ. 3 ) THEN
            Y1 = ZR(JSAUTZ+I-1) / R1
            Y2 = ZR(JSAUTZ+I-1+1) / R2
            K3 = ABS( Y1 - R1 * ( Y2 - Y1 ) / ( R2 - R1 ) )
            G = G + ( COEFG3 * K3 )
            KG1(5) = MAX ( KG1(5) , K3 )
            KG1(6) = MIN ( KG1(6) , K3 )
            KG1(7) = MAX ( KG1(7) ,  G )
            KG1(8) = MIN ( KG1(8) ,  G )
         ELSE
            KG1(5) = MAX ( KG1(5) ,  G )
            KG1(6) = MIN ( KG1(6) ,  G )
         ENDIF
            VK1 = ISIGX * SQRT(K1)
            VK2 = ISIGY * SQRT(K2)
            IF ( NDIM. EQ .2 ) THEN
               IF(NIV.EQ.2) WRITE(IFM,1010) R1 , R2 , VK1 , VK2 , G
            ELSEIF( NDIM. EQ. 3) THEN
               VK3 = ISIGZ * SQRT(K3)
               IF(NIV.EQ.2) WRITE(IFM,1010) R1, R2, VK1, VK2, VK3, G
            ENDIF
 102  CONTINUE
      KG1(1) = ISIGX * SQRT ( KG1(1) )
      KG1(2) = ISIGX * SQRT ( KG1(2) )
      KG1(3) = ISIGY * SQRT ( KG1(3) )
      KG1(4) = ISIGY * SQRT ( KG1(4) )
      IF ( NDIM .EQ. 3 ) THEN
         KG1(5) = ISIGZ * SQRT ( KG1(5) )
         KG1(6) = ISIGZ * SQRT ( KG1(6) )
      ENDIF
C
C     ------------------------------------------------------------------
C                          --- METHODE 2 ---
C     ------------------------------------------------------------------
C
      IF ( NIV .EQ. 2 )  THEN
         IF ( NDIM. EQ .2 ) THEN
            WRITE(IFM,1030)
         ELSEIF( NDIM. EQ. 3) THEN
            WRITE(IFM,1031)
         ENDIF
      ENDIF
      DO 200 I = 1 , 8 , 2
         KG2(I) = -RMAXEM()
 200  CONTINUE
      DO 210 I = 2 , 8 , 2
         KG2(I) = RMAXEM()
 210  CONTINUE
      DO 202 I = 2 , NBVAL
         R1 = ZR(JABSCS+I-1)
         Y1 = ZR(JSAUTX+I-1)
         K1 = ABS( Y1 / R1 )
         Y1 = ZR(JSAUTY+I-1)
         K2 = ABS( Y1 / R1 )
         G  = COEFG * ( K2 + K1 )
         KG2(1) = MAX ( KG2(1) , K1 )
         KG2(2) = MIN ( KG2(2) , K1 )
         KG2(3) = MAX ( KG2(3) , K2 )
         KG2(4) = MIN ( KG2(4) , K2 )
         IF ( NDIM .EQ. 3 ) THEN
            Y1 = ZR(JSAUTZ+I-1)
            K3 = ABS( Y1 / R1 )
            G = G + ( COEFG3 * K3 )
            KG2(5) = MAX ( KG2(5) , K3 )
            KG2(6) = MIN ( KG2(6) , K3 )
            KG2(7) = MAX ( KG2(7) ,  G )
            KG2(8) = MIN ( KG2(8) ,  G )
         ELSE
            KG2(5) = MAX ( KG2(5) ,  G )
            KG2(6) = MIN ( KG2(6) ,  G )
         ENDIF
            VK1 = ISIGX * SQRT(K1)
            VK2 = ISIGY * SQRT(K2)
            IF ( NDIM. EQ .2 ) THEN
            IF ( NIV .EQ. 2 ) WRITE(IFM,1010) R1 , VK1 , VK2 , G
            ELSEIF( NDIM. EQ. 3) THEN
               VK3 = ISIGZ * SQRT(K3)
               IF ( NIV .EQ. 2 ) WRITE(IFM,1010) R1, VK1, VK2,VK3, G
            ENDIF

 202  CONTINUE
      KG2(1) = ISIGX * SQRT ( KG2(1) )
      KG2(2) = ISIGX * SQRT ( KG2(2) )
      KG2(3) = ISIGY * SQRT ( KG2(3) )
      KG2(4) = ISIGY * SQRT ( KG2(4) )
      IF ( NDIM .EQ. 3 ) THEN
         KG2(5) = ISIGZ * SQRT ( KG2(5) )
         KG2(6) = ISIGZ * SQRT ( KG2(6) )
      ENDIF
C
C-----------------------------------------------------------------------
C                  METHODE 3
C
C-----------------------------------------------------------------------
C
        IF ( NIV .EQ. 2 )  THEN
         IF ( NDIM. EQ .2 ) THEN
            WRITE(IFM,1040)
         ELSEIF( NDIM. EQ. 3) THEN
            WRITE(IFM,1041)
         ENDIF
      ENDIF
      DO 300 I = 1 , 8 , 1
         KG3(I) = 0.D0
 300  CONTINUE
      DO 310 I = 2 , 8 , 2
         KG3(I) = 0.D0
 310  CONTINUE
     
       RN=ZR(JABSCS+NBVAL)
       DE=0.D0
       K1=0.D0
       K2=0.D0
       K3=0.D0
       DE=RN
       DO 302 I = 1 , NBVAL
         R1 = ZR(JABSCS+I-1)
         R2 = ZR(JABSCS+I-1+1)
         Y1 = ZR(JSAUTX+I-1)
         K1 = K1+(ABS(SQRT(Y1)*SQRT(R2)*(R2-R1)))
         Y1 = ZR(JSAUTY+I-1)
         K2 = K2+(ABS(SQRT(Y1)*SQRT(R2)*(R2-R1)))
          IF ( NDIM .EQ. 3 ) THEN
            Y1 = ZR(JSAUTZ+I-1)
            K3 = K3 +(ABS(SQRT(Y1)*SQRT(R2)*(R2-R1)))
          ENDIF   
  302    CONTINUE   
            
            VK1 = 2*ISIGX * (K1/(DE**(2)))
            VK2 = 2*ISIGY * (K2/(DE**(2)))
            G  = COEFG *(VK2**(2)+VK1**(2))
             IF ( NDIM. EQ .2 ) THEN
               IF ( NIV .EQ. 2 )WRITE(IFM,1010) VK1, VK2, G
               KG3(1)=VK1
               KG3(2)=VK1
               KG3(3)=VK2
               KG3(4)=VK2
               KG3(5)=G
               KG3(6)=G
             ELSEIF( NDIM. EQ. 3) THEN
               VK3 = 2*ISIGZ * (K3/DE**(2))
              G =COEFG*(VK2**(2)+VK1**(2))
     +         +(COEFG3*(VK3)**(2))
               IF ( NIV .EQ. 2 )WRITE(IFM,1010) VK1,VK2,VK3,G
               KG3(1)=VK1
               KG3(2)=VK1
               KG3(3)=VK2
               KG3(4)=VK2
               KG3(5)=VK3
               KG3(6)=VK3
               KG3(7)=G
               KG3(8)=G
             ENDIF       
      
      CALL JEDETR ( '&&PKCALC.SAUT_DX' )
      CALL JEDETR ( '&&PKCALC.SAUT_DY' )
      IF ( NDIM .EQ. 3 )  CALL JEDETR ( '&&PKCALC.SAUT_DZ' )
C
 1000 FORMAT(/,'    ABSC_CURV    DEPL_SUP_DX   DEPL_INF_DX     SAUT_DX',
     +         '     DEPL_SUP_DY   DEPL_INF_DY     SAUT_DY')
 1001 FORMAT(/,'    ABSC_CURV    DEPL_SUP_DX   DEPL_INF_DX     SAUT_DX',
     +         '     DEPL_SUP_DY   DEPL_INF_DY     SAUT_DY',
     +         '     DEPL_SUP_DZ   DEPL_INF_DZ     SAUT_DZ')
 1010 FORMAT(1P, 7(2X,E12.5))
 1011 FORMAT(1P,10(2X,E12.5))
 1020 FORMAT(/,'--> METHODE 1 : ',/,'   ABSC_CURV_1',
     +         '   ABSC_CURV_2        K1            K2            G')
 1021 FORMAT(/,'--> METHODE 1 : ',/,'   ABSC_CURV_1',
     +'   ABSC_CURV_2     K1              K2             K3',
     +'            G')
 1030 FORMAT(/,'--> METHODE 2 : ',/   
     +'   ABSC_CURV         K1             K2            G')
 1031 FORMAT(/,'--> METHODE 2 : ',/   
     +'   ABSC_CURV_2       K1            K2           K3',
     +'             G')
 1040 FORMAT(/,'--> METHODE 3 : ',/
     +'       K1            K2            G')
 1041 FORMAT(/,'--> METHODE 3 : ',/ 
     +'       K1            K2            K3           G')   
      
      CALL JEDEMA ()
      END

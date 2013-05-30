subroutine tensk2(icabl, nbno, s, alpha, f0,&
                  delta, ea, frco, frli, sa,&
                  f)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!  DESCRIPTION : CALCUL DE LA TENSION LE LONG D'UN CABLE EN PRENANT EN
!  -----------   COMPTE LES PERTES PAR FROTTEMENT ET LES PERTES PAR
!                RECUL DES ANCRAGES
!                CAS DE DEUX ANCRAGES ACTIFS
!                APPELANT : TENSCA
!
!  IN     : ICABL  : INTEGER , SCALAIRE
!                    NUMERO DU CABLE
!  IN     : NBNO   : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS DU CABLE
!  IN     : S      : REAL*8 , VECTEUR DE DIMENSION NBNO
!                    CONTIENT LES VALEURS DE L'ABSCISSE CURVILIGNE
!                    LE LONG DU CABLE
!  IN     : ALPHA  : REAL*8 , VECTEUR DE DIMENSION NBNO
!                    CONTIENT LES VALEURS DE LA DEVIATION ANGULAIRE
!                    CUMULEE LE LONG DU CABLE
!  IN     : F0     : REAL*8 , SCALAIRE
!                    VALEUR DE LA TENSION APPLIQUEE AUX DEUX ANCRAGES
!                    ACTIFS DU CABLE
!  IN     : DELTA  : REAL*8 , SCALAIRE
!                    VALEUR DU RECUL DES DEUX ANCRAGES
!  IN     : EA     : REAL*8 , SCALAIRE
!                    VALEUR DU MODULE D'YOUNG DE L'ACIER
!  IN     : FRCO   : REAL*8 , SCALAIRE
!                    VALEUR DU COEFFICIENT DE FROTTEMENT EN COURBE
!                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
!  IN     : FRLI   : REAL*8 , SCALAIRE
!                    VALEUR DU COEFFICIENT DE FROTTEMENT EN LIGNE
!                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
!  IN     : SA     : REAL*8 , SCALAIRE
!                    VALEUR DE L'AIRE DE LA SECTION DROITE DU CABLE
!  OUT    : F      : REAL*8 , VECTEUR DE DIMENSION NBNO
!                    CONTIENT LES VALEURS DE LA TENSION LE LONG DU CABLE
!                    APRES PRISE EN COMPTE DES PERTES PAR FROTTEMENT ET
!                    DES PERTES PAR RECUL DES DEUX ANCRAGES
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
    include 'asterfort/ancrca.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: icabl, nbno
    real(kind=8) :: s(*), alpha(*), f0, delta, ea, frco, frli, sa, f(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: ino, jabsc2, jalph2, jf1, jf2, jfmax
    real(kind=8) :: alphal, d1, d2, df, long, wcr
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   CREATION DES OBJETS DE TRAVAIL
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    call wkvect('&&TENSK2.ABSC2', 'V V R', nbno, jabsc2)
    call wkvect('&&TENSK2.ALPHA2', 'V V R', nbno, jalph2)
    call wkvect('&&TENSK2.F1', 'V V R', nbno, jf1)
    call wkvect('&&TENSK2.F2', 'V V R', nbno, jf2)
    call wkvect('&&TENSK2.FMAX', 'V V R', nbno, jfmax)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   CREATION DES DISCRETISATIONS DE L'ABSCISSE CURVILIGNE ET DE LA
!     DEVIATION ANGULAIRE CUMULEE CORRESPONDANT AU SENS DE PARCOURS
!     INVERSE LE LONG DU CABLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    long = s(nbno)
    zr(jabsc2) = 0.0d0
    do 10 ino = 2, nbno
        zr(jabsc2+ino-1) = long - s(nbno-ino+1)
10  end do
!
    alphal = alpha(nbno)
    zr(jalph2) = 0.0d0
    do 20 ino = 2, nbno
        zr(jalph2+ino-1) = alphal - alpha(nbno-ino+1)
20  end do
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   PRISE EN COMPTE DES PERTES DE TENSION PAR FROTTEMENT
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 3.1 TENSION APPLIQUEE AU PREMIER ANCRAGE ACTIF
! ---
    do 30 ino = 1, nbno
        zr(jf1+ino-1) = f0 * dble(exp(-frco*alpha(ino)-frli*s(ino)))
30  end do
!
! 3.2 TENSION APPLIQUEE AU SECOND ANCRAGE ACTIF
! ---
    do 40 ino = 1, nbno
        zr(jf2+ino-1) = f0 * dble ( exp ( -frco*zr(jalph2+ino-1) -frli*zr(jabsc2+ino-1) ))
40  end do
!
! 3.3 VALEUR MAXIMALE INDUITE PAR LES TENSIONS APPLIQUEES AUX DEUX
! --- ANCRAGES ACTIFS APRES PRISE EN COMPTE DES PERTES PAR FROTTEMENT
!
    do 50 ino = 1, nbno
        zr(jfmax+ino-1) = dble(max(zr(jf1+ino-1),zr(jf2+nbno-ino)))
50  end do
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4   PRISE EN COMPTE DES PERTES DE TENSION PAR RECUL DES DEUX ANCRAGES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 4.1 TENSION APPLIQUEE AU PREMIER ANCRAGE ACTIF
! ---
    call ancrca(icabl, nbno, s, alpha, f0,&
                delta, ea, frco, frli, sa,&
                d1, zr(jf1))
!
! 4.2 TENSION APPLIQUEE AU SECOND ANCRAGE ACTIF
! ---
    call ancrca(icabl, nbno, zr(jabsc2), zr(jalph2), f0,&
                delta, ea, frco, frli, sa,&
                d2, zr(jf2))
!
! 4.3 VALEUR FINALE INDUITE PAR LES TENSIONS APPLIQUEES AUX DEUX
! --- ANCRAGES ACTIFS APRES PRISE EN COMPTE DES PERTES PAR RECUL
!     DES DEUX ANCRAGES
!
    if (d1+d2 .lt. long) then
        do 60 ino = 1, nbno
            f(ino) = dble ( max ( zr(jf1+ino-1) , zr(jf2+nbno-ino) ) )
60      continue
    else
        do 62 ino = 1, nbno
            f(ino) = dble ( min ( zr(jf1+ino-1) , zr(jf2+nbno-ino) ) )
62      continue
    endif
!
! 4.4 CORRECTION SI RECOUVREMENT DES LONGUEURS D'APPLICATION DES PERTES
! --- DE TENSION PAR RECUL DES DEUX ANCRAGES
!
    if (d1+d2 .gt. long) then
        wcr = 0.0d0
        do 70 ino = 1, nbno-1
            wcr = wcr + (&
                  (&
                  zr(jfmax+ino-1) - f(ino) ) + ( zr(jfmax+ ino) - f(ino+1) ) ) * ( s(ino+1) - s(i&
                  &no&
                  )&
                  ) / 2.0d0
70      continue
        df = ( ea * sa * 2.0d0 * delta - wcr ) / long
        do 80 ino = 1, nbno
            f(ino) = f(ino) - df
80      continue
    endif
!
! --- MENAGE
    call jedetr('&&TENSK2.ABSC2')
    call jedetr('&&TENSK2.ALPHA2')
    call jedetr('&&TENSK2.F1')
    call jedetr('&&TENSK2.F2')
    call jedetr('&&TENSK2.FMAX')
!
    call jedema()
!
! --- FIN DE TENSK2.
end subroutine

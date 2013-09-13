subroutine pk2cau(nomte, ncmp, pk2, sigma)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!.======================================================================
    implicit none
!
!      PK2CAU  -- CALCUL DES CONTAINTES DE CAUCHY A PARTIR DES
!                 CONTRAINTES DE PIOLA-KIRCHHOFF DE SECONDE ESPECE
!                 A PARTIR DE LA FORMULE :
!
!            SIGMA = (1/DET[F])*([F]*[PK2]*[F]T)
!             OU [F] EST LA MATRICE DU GRADIENT DES DEFORMATIONS
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NOMTE          IN     K16      NOM DU TYPE D'ELEMENT
!    NCMP           IN     I        NOMBRE DE COMPOSANTES DU TENSEUR
!                                   DES CONTRAINTES
!    PK2(NCMP,1)    IN     R        TENSEUR DES CONTRAINTES
!                                   DE PIOLA-KIRCHHOFF DE SECONDE ESPECE
!    SIGMA(NCMP,1)  VAR    R        TENSEUR DES CONTRAINTES DE CAUCHY
!
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/btkb.h"
#include "asterfort/jacbm1.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/jm1dn1.h"
#include "asterfort/promat.h"
#include "asterfort/tecach.h"
#include "asterfort/utbtab.h"
#include "asterfort/utmess.h"
#include "asterfort/vectan.h"
#include "asterfort/vectgt.h"
#include "asterfort/vectpe.h"
#include "asterfort/vectrn.h"
    character(len=16) :: nomte
    integer :: ncmp, jnbspi
    real(kind=8) :: pk2(ncmp, 1), sigma(ncmp, 1)
! -----  VARIABLES LOCALES
!-----------------------------------------------------------------------
    integer :: i, icara, icompo, icou, idepl, igeom, ii
    integer :: in, inte, intsn, iret, j, kpgs, lzi
    integer :: lzr, nb1, nb2, nbcou, nbinco, npge, npgsn
!
    real(kind=8) :: cof11, cof21, cof31, detf, detfm1, detj, deux
    real(kind=8) :: epais, eptot, un, zic, zmin
!-----------------------------------------------------------------------
    parameter (nbinco=51)
    parameter (npge=3)
!
    real(kind=8) :: vecu(8, 3), vecthe(9, 3), vecta(9, 2, 3)
    real(kind=8) :: vectpt(9, 2, 3), vectn(9, 3), vecnph(9, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3), vecttt(3, 3), jm1(3, 3)
    real(kind=8) :: vecpe(nbinco), blam(9, 3, 3), bid33(3, 3)
    real(kind=8) :: xab(3, 3), dudx(3), dudy(3), dudz(3)
    real(kind=8) :: jdn1nc(9, nbinco), dudxnc(9), sigmag(3, 3)
    real(kind=8) :: ft(3, 3), sigmat(3, 3), pk2t(3, 3), pk2g(3, 3)
    real(kind=8) :: ksi3s2
!
    logical :: lgreen
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     ---------------
    un = 1.0d0
    deux = 2.0d0
!
    lgreen = .false.
!
! --- RECUPERATION DE LA CARTE DE COMPORTEMENT :
!     ----------------------------------------
    call jevech('PCOMPOR', 'L', icompo)
!
    if (zk16(icompo+2) .eq. 'GROT_GDEP') then
        lgreen = .true.
    endif
!
! --- RECUPERATION DU CHAMP DE DEPLACEMENT DANS LE CAS GROT_GDEP :
!     ---------------------------------------------------------
    if (lgreen) then
        call tecach('NNN', 'PDEPLAR', 'L', 1, idepl,&
                    iret)
        if (iret .ne. 0) then
            call tecach('NNN', 'PDEPPLU', 'L', 1, idepl,&
                        iret)
            ASSERT(iret.eq.0)
        endif
    else
        do 10 i = 1, 6
            do 10 j = 1, 270
                sigma(i,j) = pk2(i,j)
10          continue
!
        goto 9999
    endif
!
! --- RECUPERATION DES COORDONNEES DES NOEUDS DANS LA GEOMETRIE
! --- INITIALE :
!     --------
    call jevech('PGEOMER', 'L', igeom)
!
! --- CARACTERISTIQUES DE COQUES :
!     --------------------------
    call jevech('PCACOQU', 'L', icara)
! ---   EPAISSEUR TOTALE :
    eptot = zr(icara)
! ---   COORDONNEE MINIMALE SUIVANT L'EPAISSEUR
    zmin = -eptot/deux
!
! --- NOMBRE DE COUCHES :
!     -----------------
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou=zi(jnbspi-1+1)
!
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_12')
    endif
!
    if (nbcou .gt. 10) then
        call utmess('F', 'ELEMENTS_13')
    endif
!
! --- EPAISSEUR D'UNE COUCHE :
!     ----------------------
    epais = eptot/nbcou
!
! --- RECUPERATION DES OBJETS INITIALISES :
!     -----------------------------------
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
!
! --- NOMBRE DE NOEUDS (NB1 : SERENDIP, NB2 : LAGRANGE) :
!     -------------------------------------------------
    nb1 = zi(lzi+1-1)
    nb2 = zi(lzi+2-1)
!
! --- NOMBRE DE POINTS D'INTEGRATION DANS LE PLAN MOYEN
! --- (INTEGRATION NORMALE) :
!     ---------------------
    npgsn = zi(lzi+4-1)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
! --- AFFECTATION DES VECTEURS DE TRANSLATION ET DE ROTATION :
!     ------------------------------------------------------
    do 20 in = 1, nb1
        do 30 ii = 1, 3
            vecu(in,ii) = zr(idepl+6*(in-1)+ii-1)
            vecthe(in,ii) = zr(idepl+6*(in-1)+ii+3-1)
30      continue
20  end do
!
    do 40 ii = 1, 3
        vecthe(nb2,ii) = zr(idepl+6*nb1+ii-1)
40  end do
!
! --- DETERMINATION DES REPERES LOCAUX AUX NOEUDS DANS LA
! --- CONFIGURATION INITIALE
! --- VECTA DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN A
! ---       CHAQUE NOEUD
! --- VECTN DESIGNE LES VECTEURS NORMAUX AU PLAN MOYEN
! --- VECTPT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
! --- NOEUD DANS LA CONFIGURATION INITIALE :
!     ------------------------------------
    call vectan(nb1, nb2, zr(igeom), zr(lzr), vecta,&
                vectn, vectpt)
!
! --- DETERMINATION AUX NOEUDS DES VECTEURS VECNPH QUI SONT LA
! --- TRANSFORMEE APRES DEFORMATION DES VECTEURS VECTN NORMAUX
! --- AU PLAN MOYEN INITIAL ET DES MATRICES DE ROTATION BLAM FAISANT
! --- PASSER DES VECTEURS VECTN AUX VECTEURS VECNPH :
!     ---------------------------------------------
    call vectrn(nb2, vectpt, vectn, vecthe, vecnph,&
                blam)
!
! --- DETERMINATION DU VECTEUR DE DEPLACEMENT AUX NOEUDS VECPE
! --- DEFINI PAR VECPE = <U V W (NPHI-N)_X (NPHI-N)_Y (NPHI-N)_Z>
! --- OU U, V, W SONT LES 3 DDLS DE TRANSLATION
! --- NPHI EST LE VECTEUR VECNPH ET N LE VECTEUR VECTN :
!     ------------------------------------------------
    call vectpe(nb1, nb2, vecu, vectn, vecnph,&
                vecpe)
!
! --- COMPTEUR DES POINTS D'INTEGRATION :
!     ---------------------------------
    kpgs = 0
!
! --- BOUCLE SUR LES COUCHES :
!     ----------------------
    do 50 icou = 1, nbcou
!
! ---   BOUCLE SUR LES POINTS D'INTEGRATION DANS L'EPAISSEUR :
!       ----------------------------------------------------
        do 60 inte = 1, npge
!
! ---      POSITION DANS L'EPAISSEUR :
            if (inte .eq. 1) then
                zic = zmin + (icou-1)*epais
            else if (inte.eq.2) then
                zic = zmin + epais/deux + (icou-1)*epais
            else if (inte.eq.3) then
                zic = zmin + epais + (icou-1)*epais
            endif
! ---      COORDONNEE ISOPARAMETRIQUE DANS L'EPAISSEUR DIVISEE PAR 2
            ksi3s2 = zic/epais
!
! ---      BOUCLE SUR LES POINTS D'INTEGRATION DANS LE PLAN MOYEN :
!          ------------------------------------------------------
            do 70 intsn = 1, npgsn
!
                kpgs = kpgs + 1
!
! ---        DETERMINATION DES REPERES LOCAUX AUX POINTS D'INTEGRATION
! ---        DANS LA CONFIGURATION INITIALE
! ---        VECTG DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN
! ---              EN CHAQUE POINT D'INTEGRATION
! ---        VECTT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
! ---        POINT D'INTEGRATION DANS LA CONFIGURATION INITIALE :
!            --------------------------------------------------
                call vectgt(1, nb1, zr(igeom), ksi3s2, intsn,&
                            zr(lzr), epais, vectn, vectg, vectt)
!
! ---        CALCUL DE L'INVERSE DE LA MATRICE JACOBIENNE JM1:
!            ------------------------------------------------
                call jacbm1(epais, vectg, vectt, bid33, jm1,&
                            detj)
!
! ---        CALCUL DU VECTEUR JDN1NC QUI EST < DU/DQSI> (I.E.
! ---        <DU/DQSI1,DU/DQSI2,DU/DQSI3,DV/DQSI1,DV/DQSI2,DV/DQSI3,
! ---         DW/DQSI1,DW/DQSI2,DW/DQSI3> ) :
!             -----------------------------
                call jm1dn1(1, 1, nb1, nb2, zr(lzr),&
                            epais, ksi3s2, intsn, jm1, jdn1nc)
!
! ---        CALCUL DU VECTEUR DUDXNC QUI EST < DU/DX> (I.E.
! ---        <DU/DX,DU/DY,DU/DZ,DV/DX,DV/DY,DV/DZ,DW/DX,DW/DY,DW/DZ> ) :
!             ------------------------------------------------------
                call promat(jdn1nc, 9, 9, 6*nb1+3, vecpe,&
                            6*nb1+3, 6*nb1+3, 1, dudxnc)
!
                do 80 i = 1, 3
                    dudx(i) = dudxnc(1+3*(i-1))
                    dudy(i) = dudxnc(2+3*(i-1))
                    dudz(i) = dudxnc(3+3*(i-1))
80              continue
!
! ---         CONSTRUCTION DE LA MATRICE [F] DU GRADIENT DES
! ---         DEFORMATIONS AU POINT D'INTEGRATION COURANT.
! ---         PAR DEFINITION :
! ---                 | 1 0 0 |   | DU/DX DU/DY DU/DZ |
! ---           [F] = | 0 1 0 | + | DV/DX DV/DY DV/DZ |
! ---                 | 0 0 1 |   | DW/DX DW/DY DW/DZ |
! ---         PAR COMMODITE, ON UTILISE PLUTOT [FT] , LA MATRICE
! ---         TRANSPOSEE DE [F] :
!             -----------------
                do 90 i = 1, 3
                    ft(1,i) = dudx(i)
                    ft(2,i) = dudy(i)
                    ft(3,i) = dudz(i)
90              continue
!
                ft(1,1) = ft(1,1) + un
                ft(2,2) = ft(2,2) + un
                ft(3,3) = ft(3,3) + un
!
! ---         CONSTRUCTION DU TENSEUR DES CONTRAINTES PK2T A
! ---         PARTIR DU VECTEUR PK2 DES COMPOSANTES DE CE TENSEUR :
!             ---------------------------------------------------
                pk2t(1,1) = pk2(1,kpgs)
                pk2t(2,2) = pk2(2,kpgs)
                pk2t(3,3) = pk2(3,kpgs)
                pk2t(1,2) = pk2(4,kpgs)
                pk2t(1,3) = pk2(5,kpgs)
                pk2t(2,3) = pk2(6,kpgs)
                pk2t(2,1) = pk2t(1,2)
                pk2t(3,1) = pk2t(1,3)
                pk2t(3,2) = pk2t(2,3)
!
! ---         PASSAGE DU PK2 DU REPERE LOCAL A L'ELEMENT AU
! ---         REPERE GLOBAL :
!             -------------
                call btkb(3, 3, 3, pk2t, vectt,&
                          bid33, pk2g)
!
! ---         CALCUL DU TENSEUR DE CAUCHY :
!             ===========================
! ---         D'ABORD CALCUL DE [SIGMAG] = [F]*[PK2]*[FT] :
!             -------------------------------------------
                call utbtab('ZERO', 3, 3, pk2g, ft,&
                            xab, sigmag)
!
! ---         MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL :
!             --------------------------------------------------
                do 100 i = 1, 3
                    do 100 j = 1, 3
                        vecttt(i,j) = vectt(j,i)
100                  continue
!
! ---         PASSAGE DU TENSEUR DES CONTRAINTES DE CAUCHY DU
! ---         REPERE GLOBAL AU REPERE LOCAL :
!             -----------------------------
                call btkb(3, 3, 3, sigmag, vecttt,&
                          bid33, sigmat)
!
! ---         CALCUL DU DETERMINANT DE [F] ( = DET [FT] ) :
!             -------------------------------------------
                cof11 = ft(2,2)*ft(3,3) - ft(2,3)*ft(3,2)
                cof21 = ft(3,1)*ft(2,3) - ft(2,1)*ft(3,3)
                cof31 = ft(2,1)*ft(3,2) - ft(3,1)*ft(2,2)
!
                detf = ft(1,1)*cof11 + ft(1,2)*cof21 + ft(1,3)*cof31
                detfm1 = un/(detf+r8prem())
!
! ---         AFFECTATION DU VECTEUR DES COMPOSANTES DU TENSEUR
! ---         DE CAUCHY :
!             ---------
                sigma(1,kpgs) = sigmat(1,1)*detfm1
                sigma(2,kpgs) = sigmat(2,2)*detfm1
                sigma(3,kpgs) = sigmat(3,3)*detfm1
                sigma(4,kpgs) = sigmat(1,2)*detfm1
                sigma(5,kpgs) = sigmat(1,3)*detfm1
                sigma(6,kpgs) = sigmat(2,3)*detfm1
!
!
70          continue
60      continue
50  end do
!
9999  continue
!.============================ FIN DE LA ROUTINE ======================
end subroutine

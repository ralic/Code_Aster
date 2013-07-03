subroutine vdgnlr(option, nomte)
! aslint: disable=W1501
    implicit none
#include "jeveux.h"
#include "asterfort/antisy.h"
#include "asterfort/btdbma.h"
#include "asterfort/btsig.h"
#include "asterfort/gdt.h"
#include "asterfort/hsaco.h"
#include "asterfort/jacbm1.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/jm1dn1.h"
#include "asterfort/jm1dn2.h"
#include "asterfort/jm1dn3.h"
#include "asterfort/matbmn.h"
#include "asterfort/matbmr.h"
#include "asterfort/matbsr.h"
#include "asterfort/matbsu.h"
#include "asterfort/matrc2.h"
#include "asterfort/moytpg.h"
#include "asterfort/promat.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rogllo.h"
#include "asterfort/tilbar.h"
#include "asterfort/transp.h"
#include "asterfort/u2mess.h"
#include "asterfort/vectan.h"
#include "asterfort/vectgt.h"
#include "asterfort/vectpe.h"
#include "asterfort/vectrn.h"
#include "asterfort/verifg.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
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
! ......................................................................
!     FONCTION  :  CALCUL DES OBJETS ELEMENTS FINIS EN NON LINEAIRE
!                  GEOMETRIQUE AVEC GRANDES ROTATIONS
!                  COQUE_3D
!
!     ARGUMENTS :
!     DONNEES   :      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
!     OPTIONS   :
!                  RIGI_MECA_TANG : MATRICE TANGENTE DE RIGDITE
!                                   PHASE DE PREDICTION AU DEBUT DE
!                                   CHAQUE PAS
!
!                  RAPH_MECA      : CONTRAINTES CAUCHY ET FORCE INTERNE
!                                   SANS MATRICE TANGENTE DE RIGIDITE
!                                   REAC_ITER : 0 DANS STAT_NON_LINE
!
!                  FULL_MECA      : RAPH_MECA + RIGI_MECA_TANG
!                                   ITERATION TYPIQUE DE NEWTON
!
! ......................................................................
!
!
!---- DECLARATIONS BIDONS
!
    real(kind=8) :: bid33 ( 3 , 3 )
!
!---- DECLARATIONS LOCALES
!
    integer :: i, j
    integer :: in
    integer :: jd
    integer :: ii, jj
!
!---- DECLARATIONS RIGIDITE GEOMETRIQUE
!
    real(kind=8) :: etild ( 5 ), stild ( 5 )
    real(kind=8) :: stlis ( 5 , 4 )
    real(kind=8) :: bars ( 9 , 9 )
    real(kind=8) :: vecni ( 3 ), antni ( 3 , 3 )
    real(kind=8) :: veczn ( 27 )
    real(kind=8) :: antzi ( 3 , 3 )
    real(kind=8) :: rignc ( 3 , 3 )
!
!---- DECLARATIONS STANDARDS
!
    integer :: igeom, icontp, imatun, ivectu, ivarip
    integer :: lzi, lzr, jcara
    integer :: nb1, nb2, nbpar
    integer :: iinstm, iinstp, jmate
    real(kind=8) :: valpar, epsthe
    integer :: icodre
    character(len=8) :: nompar
    character(len=10) :: phenom
!
!---- DECLARATIONS PROPRES COQUE_3D NON LINEAIRE
!
    real(kind=8) :: matc ( 5 , 5 )
    integer :: inte, intsr, intsn, jnbspi
    integer :: kntsr
    real(kind=8) :: eptot, kappa, ctor
    integer :: npge, npgsr, npgsn
    parameter ( npge = 3 )
    real(kind=8) :: vecta ( 9 , 2 , 3 )
    real(kind=8) :: vectn ( 9 , 3 ), vectpt ( 9 , 2 , 3 )
    real(kind=8) :: vecnph ( 9 , 3 )
    real(kind=8) :: vectg ( 2 , 3 ), vectt ( 3 , 3 )
    real(kind=8) :: jm1 ( 3 , 3 ), detj
    real(kind=8) :: hsc ( 5 , 9 )
    real(kind=8) :: jdn1ri ( 9 , 51 ), jdn1rc ( 9 , 51 )
    real(kind=8) :: jdn1ni ( 9 , 51 ), jdn1nc ( 9 , 51 )
    real(kind=8) :: jdn2rc ( 9 , 51 )
    real(kind=8) :: jdn2nc ( 9 , 51 )
    real(kind=8) :: j1dn3 ( 9 , 27 )
    real(kind=8) :: btild3 ( 5 , 27 )
    real(kind=8) :: ksi3s2
!
!---- DECLARATIONS COUCHES
!
    integer :: icompo, nbcou, icou, k1
    real(kind=8) :: zic, zmin, epais, coef
!
!---- DECLARATIONS COQUE NON LINEAIRE
!
    real(kind=8) :: vrignc ( 2601 ), vrigni ( 2601 )
    real(kind=8) :: vrigrc ( 2601 ), vrigri ( 2601 )
    real(kind=8) :: knn
    integer :: iup, ium, iud, iret, iret1
    real(kind=8) :: b1su ( 5 , 51 ), b2su ( 5 , 51 )
    real(kind=8) :: b1src ( 2 , 51 , 4 )
    real(kind=8) :: b2src ( 2 , 51 , 4 )
    real(kind=8) :: b1mnc ( 3 , 51 ), b1mni ( 3 , 51 )
    real(kind=8) :: b2mnc ( 3 , 51 ), b2mni ( 3 , 51 )
    real(kind=8) :: b1mri ( 3 , 51 , 4 )
    real(kind=8) :: b2mri ( 3 , 51 , 4 )
    real(kind=8) :: dudxri ( 9 ), dudxni ( 9 )
    real(kind=8) :: dudxrc ( 9 ), dudxnc ( 9 )
    real(kind=8) :: vecu ( 8 , 3 ), vecthe ( 9 , 3 )
    real(kind=8) :: vecpe ( 51 )
!
!---- DECLARATIONS ROTATION GLOBAL LOCAL AU NOEUDS
!
!
    real(kind=8) :: blam ( 9 , 3 , 3 )
!
    real(kind=8) :: theta ( 3 ), thetan
    real(kind=8) :: tmoin1 ( 3 , 3 ), tm1t ( 3 , 3 )
    real(kind=8) :: term ( 3 )
!
! DEB
!
!
!______________________________________________________________________
!
!---- CALCUL COMMUNS A TOUTES LES OPTIONS
!______________________________________________________________________
!
!---- LE NOMBRE DE COUCHES
!
    call jevech('PCOMPOR', 'L', icompo)
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou=zi(jnbspi-1+1)
!
    if (nbcou .le. 0) call u2mess('F', 'ELEMENTS_12')
!
    if (nbcou .gt. 10) call u2mess('F', 'ELEMENTS_13')
!______________________________________________________________________
!
!---- RECUPERATION DES POINTEURS ( L : LECTURE )
!______________________________________________________________________
!
!....... GEOMETRIE INITIALE ( COORDONNEES INITIALE DES NOEUDS )
!
    call jevech('PGEOMER', 'L', igeom)
!
!---- RECUPERATION DES OBJETS INITIALISES
!
!....... LES ENTIERS
!
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
!
!------- NOMBRE DE NOEUDS ( NB1 : SERENDIP , NB2 : LAGRANGE )
!
    nb1 = zi ( lzi - 1 + 1 )
    nb2 = zi ( lzi - 1 + 2 )
!
!------- NBRE POINTS INTEGRATIONS ( NPGSR : REDUITE , NPGSN : NORMALE )
!
    npgsr = zi ( lzi - 1 + 3 )
    npgsn = zi ( lzi - 1 + 4 )
!
!....... LES REELS ( FONCTIONS DE FORMES, DERIVEES ET POIDS )
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
!______________________________________________________________________
!
!---- POUR LE CALCUL DES DEFORMATIONS THERMIQUES
!______________________________________________________________________
!
    call jevech('PMATERC', 'L', jmate)
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
!______________________________________________________________________
!
!---- RECUPERATION DES POINTEURS ( E : ECRITURE ) SELON OPTION
!______________________________________________________________________
!
    if (option ( 1 : 9 ) .eq. 'RAPH_MECA' .or. option ( 1 : 9 ) .eq. 'FULL_MECA') then
!
!------- CONTRAINTES DE CAUCHY AUX POINTS DE GAUSS
!
        call jevech('PCONTPR', 'E', icontp)
!
!------- VECTEUR DES FORCES INTERNES
!
        call jevech('PVECTUR', 'E', ivectu)
!
!------- VARIABLES INTERNES INACTIVES DANS NOTRE CAS
!
        call jevech('PVARIPR', 'E', ivarip)
!
    endif
!
!
    if (option ( 1 : 9 ) .eq. 'RAPH_MECA' .or. option ( 1 : 9 ) .eq. 'FULL_MECA') then
!
!------- CONTRAINTES DE CAUCHY AUX POINTS DE GAUSS
!
        call jevech('PCONTPR', 'E', icontp)
!
!------- VECTEUR DES FORCES INTERNES
!
        call jevech('PVECTUR', 'E', ivectu)
!
!------- VARIABLES INTERNES INACTIVES DANS NOTRE CAS
!
        call jevech('PVARIPR', 'E', ivarip)
!
    endif
!
    if (option ( 1 : 16 ) .eq. 'RIGI_MECA_TANG' .or. option ( 1 : 9 ) .eq. 'FULL_MECA') then
!
!------- MATRICE TANGENTE DE RIGIDITE ET INITIALISATION
!
        call jevech('PMATUNS', 'E', imatun)
!
!------- INITIALISATION DES MATRICES GEOMETRIQUES
!
!------- NORMAL   COMPLET ( CONTRAINTES MEMBRANE FLEXION )
!
        call r8inir(51 * 51, 0.d0, vrignc, 1)
!
!------- NORMAL INCOMPLET ( CONTRAINTES MEMBRANE FLEXION )
!
        call r8inir(51 * 51, 0.d0, vrigni, 1)
!
!------- REDUIT INCOMPLET ( CONTRAINTES SHEAR            )
!
        call r8inir(51 * 51, 0.d0, vrigri, 1)
!
!------- REDUIT   COMPLET ( CONTRAINTES SHEAR            )
!
        call r8inir(51 * 51, 0.d0, vrigrc, 1)
!
!------- INITIALISATION DE VECZN AVANT INTEGRATION
!
        call r8inir(27, 0.d0, veczn, 1)
!
    endif
!______________________________________________________________________
!
!---- CARACTERISTIQUES DE COQUE
!
    call jevech('PCACOQU', 'L', jcara)
!
!---- EPAISSEUR TOTALE
!
    eptot = zr ( jcara )
!
!---- COEFFICIENT DE CORRECTION DU SHEAR
!
    kappa = zr ( jcara + 3 )
!
!---- COEFFICIENT DE RIGIDITE AUTOUR DE LA TRANSFORMEE DE LA NORMALE
!
    ctor = zr ( jcara + 4 )
!
!---- COORDONNEE MINIMALE SUIVANT L EPAISSEUR
!
    zmin = - eptot / 2.d0
!
!---- EPAISSEUR D UNE COUCHE
!
    epais = eptot / nbcou
!
!______________________________________________________________________
!
!---- RECUPERATION DE L ADRESSE DES VARIABLES NODALES TOTALES
!     QUI NE POSSEDE PAS LE MEME SENS POUR LES DEPLACEMENTS
!     ET LES ROTATIONS
!
!---- A L INSTANT MOINS  ( PAS PRECEDENT )
!
    call jevech('PDEPLMR', 'L', ium)
!
!---- A L INSTANT PLUS  ( DEPUIS LE PAS PRECEDENT PAS PRECEDENT )
!
    call jevech('PDEPLPR', 'L', iup)
!______________________________________________________________________
!
!---- ENTRE DEUX ITERATIONS
!
    call jevech('PDDEPLA', 'L', iud)
!
!______________________________________________________________________
!
!
!---- REPERE LOCAUX AUX NOEUDS SUR LA CONFIGURATION INITIALE
!
    call vectan(nb1, nb2, zr(igeom), zr(lzr), vecta,&
                vectn, vectpt)
!
!---- DEPLACEMENT TOTAL AUX NOEUDS DE SERENDIP
!
    call r8inir(8 * 3, 0.d0, vecu, 1)
!
    do 101 in = 1, nb1
        do 111 ii = 1, 3
!
            vecu ( in , ii ) = zr ( ium - 1 + 6 * ( in - 1 ) + ii&
            ) + zr ( iup - 1 + 6 * ( in - 1 ) + ii )
!
111      continue
101  end do
!
!---- ROTATION TOTALE AUX NOEUDS
!
    call r8inir(9 * 3, 0.d0, vecthe, 1)
!
    if (zk16 ( icompo ) ( 1 : 4 ) .eq. 'ELAS') then
!
!------- EN ACCORD AVEC LA MISE A JOUR DES GRANDES ROTATIONS AUFAURE
!
!------- NOEUD DE SERENDIP
!
        do 201 in = 1, nb1
            do 211 ii = 1, 3
                vecthe ( in , ii ) = zr ( iup - 1 + 6 * ( in - 1 ) +&
                ii + 3 )
211          continue
201      continue
!
!------- SUPERNOEUD
!
        do 221 ii = 1, 3
            vecthe ( nb2, ii ) = zr ( iup - 1 + 6 * ( nb1 ) + ii&
            )
221      continue
!
    else
!
!------- EN ACCORD AVEC LA MISE A JOUR CLASSIQUE DE STAT_NON_LINE
!
!------- NOEUDS DE SERENDIP
!
        do 202 in = 1, nb1
            do 212 ii = 1, 3
                vecthe ( in , ii ) = zr ( ium - 1 + 6 * ( in - 1 ) +&
                ii + 3 ) + zr ( iup - 1 + 6 * ( in - 1 ) + ii + 3 )
212          continue
202      continue
!
!--------- SUPERNOEUD
!
        do 222 ii = 1, 3
            vecthe ( nb2, ii ) = zr ( ium - 1 + 6 * ( nb1 ) + ii&
            ) + zr ( iup - 1 + 6 * ( nb1 ) + ii )
222      continue
!
    endif
!
!---- TRANSFORMEES NORMALES ET MATRICES DE ROTATION AUX NOEUDS
!
    call vectrn(nb2, vectpt, vectn, vecthe, vecnph,&
                blam)
!
!---- VECTEUR PE DES VARIABLES NODALES TOTALES GENERALISEES
!
    call vectpe(nb1, nb2, vecu, vectn, vecnph,&
                vecpe)
!
!______________________________________________________________________
!
!---- INITIALISATION DES OPERATEURS DE DEFORMATION A EXTRAPOLER
!
!---- MEMBRANE REDUIT INCOMPLET
!
    call r8inir(3 * 51 * 4, 0.d0, b1mri, 1)
!
    call r8inir(3 * 51 * 4, 0.d0, b2mri, 1)
!
!---- SHEAR    REDUIT   COMPLET
!
    call r8inir(2 * 51 * 4, 0.d0, b1src, 1)
!
    call r8inir(2 * 51 * 4, 0.d0, b2src, 1)
!
!---- COMPTEUR DES POINTS D INTEGRATIONS ( EPAISSEUR * SURFACE )
!
!
!==== BOUCLE SUR LES COUCHES
!
    do 600 icou = 1, nbcou
!
!======= BOUCLE SUR LES POINTS D INTEGRATION SUR L EPAISSEUR
!
        do 610 inte = 1, npge
!
!---------- POSITION SUR L EPAISSEUR ET POIDS D INTEGRATION
!
            if (inte .eq. 1) then
!
                zic = zmin + ( icou - 1 ) * epais
!
                coef = 1.d0 / 3.d0
!
            else if (inte .eq. 2) then
!
                zic = zmin + epais / 2.d0 + ( icou - 1 ) * epais
!
                coef = 4.d0 / 3.d0
!
            else
!
                zic = zmin + epais + ( icou - 1 ) * epais
!
                coef = 1.d0 / 3.d0
!
            endif
!
!---------- COORDONNEE ISOP.  SUR L EPAISSEUR  DIVISEE PAR DEUX
!
            ksi3s2 = zic / epais
!
!========== 1 ERE BOUCLE SUR POINTS INTEGRATION REDUITE SURFACE MOYENNE
!
            do 620 intsr = 1, npgsr
!
                call vectgt(0, nb1, zr ( igeom ), ksi3s2, intsr,&
                            zr ( lzr ), epais, vectn, vectg, vectt)
!
                call jacbm1(epais, vectg, vectt, bid33, jm1,&
                            detj)
!
!------------- J1DN1RI ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
!                                          INDC = 0 INCOMPLET
                call jm1dn1(0, 0, nb1, nb2, zr ( lzr ),&
                            epais, ksi3s2, intsr, jm1, jdn1ri)
!
!------------- CALCUL DE    DUDXRI ( 9 ) REDUIT INCOMPLET
!
                call promat(jdn1ri, 9, 9, 6 * nb1 + 3, vecpe,&
                            6 * nb1 + 3, 6 * nb1 + 3, 1, dudxri)
!
!+++++++++++++ B1MRI ( 3 , 51 , 4 ) MEMBRANE REDUIT INCOMPLET
!              B2MRI ( 3 , 51 , 4 )
!
                call matbmr(nb1, vectt, dudxri, intsr, jdn1ri,&
                            b1mri, b2mri)
!
!------------- J1DN1RC ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
!                                          INDC = 1 COMPLET
!
                call jm1dn1(0, 1, nb1, nb2, zr ( lzr ),&
                            epais, ksi3s2, intsr, jm1, jdn1rc)
!
!------------- CALCUL DE    DUDXRC ( 9 ) REDUIT COMPLET
!
                call promat(jdn1rc, 9, 9, 6 * nb1 + 3, vecpe,&
                            6 * nb1 + 3, 6 * nb1 + 3, 1, dudxrc)
!
!------------- J1DN2RC ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
!                                          INDC = 1 COMPLET
!
                call jm1dn2(0, 1, nb1, nb2, zr ( lzr ),&
                            epais, ksi3s2, intsr, vecnph, jm1,&
                            jdn2rc)
!
!+++++++++++++ B1SRC ( 2 , 51 , 4 ) SHEAR REDUIT COMPLET
!              B2SRC ( 2 , 51 , 4 )
!
                call matbsr(nb1, vectt, dudxrc, intsr, jdn1rc,&
                            jdn2rc, b1src, b2src)
!
!========== FIN 1 ERE BOUCLE NPGSR
!
620          continue
!
!---------- INITIALISATION DES CONTRAINTES A LISSER
!
            if (option ( 1 : 16 ) .eq. 'RIGI_MECA_TANG' .or. option ( 1 : 9 ) .eq. 'FULL_MECA') &
            call r8inir(5 * 4, 0.d0, stlis, 1)
!
!========== BOUCLE SUR POINTS INTEGRATION NORMALE SURFACE MOYENNE
!
            do 630 intsn = 1, npgsn
!
!
                call vectgt(1, nb1, zr ( igeom ), ksi3s2, intsn,&
                            zr ( lzr ), epais, vectn, vectg, vectt)
!
                call jacbm1(epais, vectg, vectt, bid33, jm1,&
                            detj)
!
!------------- J1DN1NC ( 9 , 6 * NB1 + 3 ) INDN = 1 NORMAL
!                                          INDC = 1 COMPLET
!
                call jm1dn1(1, 1, nb1, nb2, zr ( lzr ),&
                            epais, ksi3s2, intsn, jm1, jdn1nc)
!
!------------- CALCUL DE     DUDXNC ( 9 ) NORMAL COMPLET
!
                call promat(jdn1nc, 9, 9, 6 * nb1 + 3, vecpe,&
                            6 * nb1 + 3, 6 * nb1 + 3, 1, dudxnc)
!
!------------- J1DN2NC ( 9 , 6 * NB1 + 3 ) INDN = 1 NORMAL
!                                          INDC = 1 COMPLET
!
                call jm1dn2(1, 1, nb1, nb2, zr ( lzr ),&
                            epais, ksi3s2, intsn, vecnph, jm1,&
                            jdn2nc)
!
!+++++++++++++ B1MNC ( 3 , 51 ) MEMBRANE NORMAL COMPLET
!              B2MNC ( 3 , 51 )
!
                call matbmn(nb1, vectt, dudxnc, jdn1nc, jdn2nc,&
                            b1mnc, b2mnc)
!
!------------- J1DN1NI ( 9 , 6 * NB1 + 3 ) INDN = 1 NORMAL
!                                          INDC = 0 INCOMPLET
!
                call jm1dn1(1, 0, nb1, nb2, zr ( lzr ),&
                            epais, ksi3s2, intsn, jm1, jdn1ni)
!
!------------- CALCUL DE     DUDXNI ( 9 ) NORMAL INCOMPLET
!
                call promat(jdn1ni, 9, 9, 6 * nb1 + 3, vecpe,&
                            6 * nb1 + 3, 6 * nb1 + 3, 1, dudxni)
!
!+++++++++++++ B1MNI ( 3 , 51 ) MEMBRANE NORMAL INCOMPLET
!              B2MNI ( 3 , 51 )
!
                call matbmn(nb1, vectt, dudxni, jdn1ni, jdn1ni,&
                            b1mni, b2mni)
!
!============= B1SU ( 5 , 51 ) SUBSTITUTION TOTAL
!              B2SU ( 5 , 51 ) SUBSTITUTION DIFFERENTIEL
!
                call matbsu(nb1, zr ( lzr ), npgsr, intsn, b1mnc,&
                            b2mnc, b1mni, b2mni, b1mri, b2mri,&
                            b1src, b2src, b1su, b2su)
!
!------------- LA  DEFORMATION TOTALE  DE GREEN LAGRANGE ETILD ( 5 )
!
                call promat(b1su, 5, 5, 6 * nb1 + 3, vecpe,&
                            6 * nb1 + 3, 6 * nb1 + 3, 1, etild)
!
!------------- EVALUATION DES DEFORMATIONS THERMIQUES
!
                call verifg('RIGI', intsn, 3, '+', zi(jmate),&
                            phenom, 1, epsthe, iret1)
                etild(1) = etild(1) - epsthe
                etild(2) = etild(2) - epsthe
!
!------------- LA  MATRICE DE COMPORTEMENT  MATC ( 5 , 5 )
!
                call moytpg('RIGI', intsn, 3, '+', valpar,&
                            iret)
                nbpar = 1
                nompar = 'TEMP'
                call matrc2(nbpar, nompar, valpar, kappa, matc,&
                            vectt)
!
!------------- LA  CONTRAINTE TOTALE  PK2 STILD ( 5 )
!
                call promat(matc, 5, 5, 5, etild,&
                            5, 5, 1, stild)
!
                if (option ( 1 : 9 ) .eq. 'RAPH_MECA' .or. option ( 1 : 9 ) .eq.&
                    'FULL_MECA') then
!
!------- CONTRAINTES DE CAUCHY = PK2 AUX POINTS DE GAUSS
!
                    k1=6*((intsn-1)*npge*nbcou + (icou-1)*npge +inte -&
                    1)
                    zr ( icontp - 1 + k1 + 1 ) = stild ( 1 )
                    zr ( icontp - 1 + k1 + 2 ) = stild ( 2 )
!
                    zr ( icontp - 1 + k1 + 3 ) = 0.d0
!
                    zr ( icontp - 1 + k1 + 4 ) = stild ( 3 )
!
                    zr ( icontp - 1 + k1 + 5 ) = stild ( 4 )
                    zr ( icontp - 1 + k1 + 6 ) = stild ( 5 )
!
!------------- FINT ( 6 * NB1 + 3 )  =     INTEGRALE  DE
!              ( B2SU ( 5 , 6 * NB1 + 3 ) ) T * STILD ( 5 ) *
!              POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
!
                    call btsig(6 * nb1 + 3, 5, zr (lzr - 1 + 127 + intsn - 1) * detj * coef,&
                               b2su, stild, zr ( ivectu ))
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!------------- VARIABLES INTERNES INACTIVES COMPORTEMENT NON PLASTIQUE
!
                endif
!
!
                if (option ( 1 : 16 ) .eq. 'RIGI_MECA_TANG' .or. option ( 1 : 9 ) .eq.&
                    'FULL_MECA') then
!
!------------- INTEGRATION DES CONTRAINTES LISSEES
!
                    do 700 kntsr = 1, npgsr
!
                        do 710 i = 1, 5
!
                            stlis ( i , kntsr ) = stlis ( i , kntsr )&
                            + zr ( lzr - 1 + 702 + 4 * ( intsn - 1 ) +&
                            kntsr ) * stild ( i ) * zr ( lzr - 1 +&
127                           + intsn - 1 )
!
710                      continue
700                  continue
!
!------------- KM ( 6 * NB1 + 3 , 6 * NB1 + 3 )  =     INTEGRALE  DE
!                ( B2SU ( 5 , 6 * NB1 + 3 ) ) T * MATC ( 5 , 5 ) *
!                  B2SU ( 5 , 6 * NB1 + 3 )
!                POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
!
                    call btdbma(b2su, matc, zr (lzr - 1 + 127 + intsn - 1) * detj * coef, 5,&
                                6 * nb1 + 3, zr ( imatun ))
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!RR
!RR   RIGIDITE GEOMETRIQUE NON CLASSIQUE TOUT
!RR
!
!---------- POUR LE TERME NON CLASSIQUE
!           HSC ( 5 , 9 ) = H ( 5 , 6 )  * S ( 6 , 9 )
!
                    call hsaco(vectt, dudxnc, hsc)
!
!---------- CALCUL DE
!           J1DN3( 9 , 3 * NB2 )=JTILDM1( 9 , 9 )*DNDQSI3( 9 , 3 * NB2 )
!
                    call jm1dn3(nb2, zr ( lzr ), epais, ksi3s2, intsn,&
                                jm1, j1dn3)
!---------- CALCUL DE
!           BTILD3 ( 5 , 27 ) = HSC ( 5 , 9 ) * J1DN3 ( 9 , 3 * NB2 )
!
                    call promat(hsc, 5, 5, 9, j1dn3,&
                                9, 9, 3 * nb2, btild3)
!
!---------- VECZN ( 27 )  =     INTEGRALE  DE
!           ( BTILD3 ( 5 , 27 ) ) T * STILD ( 5 ) *
!           POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
!
                    call btsig(3 * nb2, 5, zr (lzr - 1 + 127 + intsn - 1) * detj * coef, btild3,&
                               stild, veczn)
!
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!----------------------------------------------------------------------
!RR
!RR   RIGIDITE GEOMETRIQUE CLASSIQUE MEMBRANE FLEXION
!RR
!
!------------- ANNULATION DU SHEAR
!---------------------------------
!
                    call r8inir(2, 0.d0, stild ( 4 ), 1)
!
!------------- BARS ( 9 , 9 )
!
                    call tilbar(stild, vectt, bars)
!
!------------- VRIGNC  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTEGRALE
!              ( JDN2NC ( 9 , 6 * NB1 + 3 ) ) T * BARS   ( 9 , 9 )
!           *                               JDN2NC ( 9 , 6 * NB1 + 3 ) *
!              POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
!
                    call btdbma(jdn2nc, bars, zr (lzr - 1 + 127 + intsn - 1) * detj * coef, 9,&
                                6 * nb1 + 3, vrignc)
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!------------- VRIGNI  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTEGRALE
!              ( JDN1NI ( 9 , 6 * NB1 + 3 ) ) T * BARS   ( 9 , 9 )
!           *                               JDN1NI ( 9 , 6 * NB1 + 3 ) *
!              POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
!
                    call btdbma(jdn1ni, bars, zr (lzr - 1 + 127 + intsn - 1) * detj * coef, 9,&
                                6 * nb1 + 3, vrigni)
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
                endif
!
!========== FIN BOUCLE NPGSN
!
630          continue
!
            if (option ( 1 : 16 ) .eq. 'RIGI_MECA_TANG' .or. option ( 1 : 9 ) .eq.&
                'FULL_MECA') then
!
!========== 2 EME BOUCLE SUR POINTS INTEGRATION REDUITE SURFACE MOYENNE
!
                do 640 intsr = 1, npgsr
!
                    call vectgt(0, nb1, zr ( igeom ), ksi3s2, intsr,&
                                zr ( lzr ), epais, vectn, vectg, vectt)
!
                    call jacbm1(epais, vectg, vectt, bid33, jm1,&
                                detj)
!
!------------- J1DN1RI ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
!                                          INDC = 0 INCOMPLET
!
                    call jm1dn1(0, 0, nb1, nb2, zr ( lzr ),&
                                epais, ksi3s2, intsr, jm1, jdn1ri)
!
!
!IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
!
!------------- RESTITUTION DES CONTRAINTES LISSEES MEMBRANE FLEXION
!
                    do 800 i = 1, 3
                        stild ( i ) = stlis ( i , intsr )
800                  continue
!
!------------- ANNULATION DU SHEAR
!
                    call r8inir(2, 0.d0, stild ( 4 ), 1)
!
!------------- BARS ( 9 , 9 )
!
                    call tilbar(stild, vectt, bars)
!
!------------- VRIGRI  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTEGRALE
!              ( JDN1RI ( 9 , 6 * NB1 + 3 ) ) T * BARS   ( 9 , 9 )
!           *                               JDN1RI ( 9 , 6 * NB1 + 3 ) *
!                                      DETJ * POIDS EPAISSEUR
!DDDDDDDDDDDDD
!------------- PAS D INTEGRATION REDUITE SURFACE MOYENNE
!DDDDDDDDDDDDD
!
                    call btdbma(jdn1ri, bars, detj * coef, 9, 6 * nb1 + 3,&
                                vrigri)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!------------- J1DN2RC ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
!                                          INDC = 1 COMPLET
!
                    call jm1dn2(0, 1, nb1, nb2, zr ( lzr ),&
                                epais, ksi3s2, intsr, vecnph, jm1,&
                                jdn2rc)
!
!------------- ANNULATION DE MEMBRANE FLEXION
!
                    call r8inir(3, 0.d0, stild ( 1 ), 1)
!
!------------- RESTITUTION DES CONTRAINTES LISSEES DE SHEAR
!
                    do 850 i = 4, 5
                        stild ( i ) = stlis ( i , intsr )
850                  continue
!
!------------- BARS ( 9 , 9 )
!
                    call tilbar(stild, vectt, bars)
!
!------------- VRIGRC  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTEGRALE
!              ( JDN2RC ( 9 , 6 * NB1 + 3 ) ) T * BARS   ( 9 , 9 )
!           *                               JDN2RC ( 9 , 6 * NB1 + 3 ) *
!              POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
!
!DDDDDDDDDDDDD
!------------- PAS D INTEGRATION REDUITE SURFACE MOYENNE
!DDDDDDDDDDDDD
!
                    call btdbma(jdn2rc, bars, detj * coef, 9, 6 * nb1 + 3,&
                                vrigrc)
!
!========== FIN 2 EME BOUCLE NPGSR
!
640              continue
!
            endif
!
!-------- FIN BOUCLE NPGE
!
610      continue
!
!---- FIN BOUCLE NBCOU
!
600  end do
!
    if (option ( 1 : 16 ) .eq. 'RIGI_MECA_TANG' .or. option ( 1 : 9 ) .eq. 'FULL_MECA') then
!
!------- AFFECTATION DE LA RIGIDITE GEOMETRIQUE
!
        do 400 jd = 1, ( 6 * nb1 + 3 ) * ( 6 * nb1 + 3 )
            zr ( imatun - 1 + jd ) = zr ( imatun - 1 + jd ) + vrignc (&
            jd ) - vrigni ( jd ) + vrigri ( jd ) + vrigrc ( jd )
!
400      continue
!
!------- AFFECTATION DE LA RIGIDITE NON CLASSIQUE RIGNC ( 3 , 3 )
!
        do 500 in = 1, nb2
!
!---------- MATRICE ANTISYMETRIQUE    ANTZI ( 3 , 3 ) AU NOEUD
!
            call antisy(veczn ( ( in - 1 ) * 3 + 1 ), 1.d0, antzi)
!
!---------- TRANSFOR DE NORMALE ET SA MATRICE ANTISYM AU NOEUD
!
            do 520 ii = 1, 3
                vecni ( ii ) = vecnph ( in , ii )
520          continue
!
            call antisy(vecni, 1.d0, antni)
!
!---------- RIGIDITE NON CLASSIQUE RIGN ( 3 , 3 ) NON SYMETRIQUE
!
            call promat(antzi, 3, 3, 3, antni,&
                        3, 3, 3, rignc)
!
!--------- RIGIDITE NON CLASSIQUE DESACTIVEE
!
!            IF ( IN . LE . NB1 ) THEN
!
!------------- NOEUDS DE SERENDIP
!
!               DO 531 JJ = 1 , 3
!                  J    = 6 * ( IN - 1 ) + JJ + 3
!                  DO 541 II = 1 , 3
!                     I    = 6 * ( IN - 1 ) + II + 3
!                     IRIG = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
!                     ZR ( IMATUN-1 + IRIG ) = ZR ( IMATUN-1 + IRIG  )
!     &       +  RIGNC ( II , JJ )
!
! 541              CONTINUE
! 531           CONTINUE
!
!            ELSE
!
!------------- SUPERNOEUD
!               DO 532 JJ = 1 , 3
!                  J    = 6 * NB1        + JJ
!                  DO 542 II = 1 , 3
!                     I    = 6 * NB1        + II
!                     IRIG = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
!                     ZR ( IMATUN-1 + IRIG ) = ZR ( IMATUN-1 + IRIG  )
!     &       +  RIGNC ( II , JJ )
!
! 542              CONTINUE
! 532           CONTINUE
!
!            ENDIF
!
500      continue
!
!------- ROTATION DE TOUTE LA MATRICE AU REPERE LOCAL
!
        call rogllo(nb1, nb2, zr ( imatun ), blam, ctor,&
                    knn)
!
    else
!
!++++ MATRICE ELASTIQUE
!
        knn = 0.d0
!
    endif
!
!++++ SECOND MEMBRE DES FORCES INTERIEURES
!
!++++++++ BOUCLE SUR LES NOEUDS DE ROTATION
!
    do 900 in = 1, nb2
!
!+++++++++++ ROTATION AUTOUR DE LA NORMALE INITIALE
!
        do 910 ii = 1, 3
            vecni ( ii ) = vectn ( in , ii )
            theta ( ii ) = vecthe ( in , ii )
910      continue
!
        thetan=ddot(3,theta,1,vecni,1)
!
!+++++++++++ MATRICE T MOIUNS 1 DE THETA
!
        call gdt(theta, tmoin1)
!
!
!
!+++++++++++ SON TRANSPOSE
!
        call transp(tmoin1, 3, 3, 3, tm1t,&
                    3)
!
!+++++++++++ PRODUIT T MOINS 1 T FOIS VECNI
!
        call promat(tm1t, 3, 3, 3, vecni,&
                    3, 3, 1, term)
!
!
!
!
        if (option ( 1 : 16 ) .eq. 'RIGI_MECA_TANG' .or. option ( 1 : 9 ) .eq. 'FULL_MECA') then
!
            if (in .le. nb1) then
!
!-------------- AFFECTATION
!
!-------------- NOEUDS DE SERENDIP
                do 331 jj = 1, 3
                    j = 6 * ( in - 1 ) + jj + 3
                    do 341 ii = 1, 3
                        i = 6 * ( in - 1 ) + ii + 3
                        zr ( imatun - 1 + ( 6 * nb1 + 3 ) * ( j - 1 )&
                        + i ) = zr ( imatun - 1 + ( 6 * nb1 + 3 ) * (&
                        j - 1 ) + i ) + knn * term ( ii ) * term&
                        ( jj )
341                  continue
331              continue
!
            else
!
!-------------- SUPERNOEUD
                do 351 jj = 1, 3
                    j = 6 * nb1 + jj
                    do 361 ii = 1, 3
                        i = 6 * nb1 + ii
                        zr ( imatun - 1 + ( 6 * nb1 + 3 ) * ( j - 1 )&
                        + i ) = zr ( imatun - 1 + ( 6 * nb1 + 3 ) * (&
                        j - 1 ) + i ) + knn * term ( ii ) * term&
                        ( jj )
361                  continue
351              continue
!
            endif
!
        endif
!
!
!
        if (option ( 1 : 9 ) .eq. 'RAPH_MECA' .or. option ( 1 : 9 ) .eq. 'FULL_MECA') then
!
            if (in .le. nb1) then
!
                do 920 ii = 1, 3
!
                    zr ( ivectu - 1 + 6 * ( in - 1 ) + ii + 3 ) =&
                    zr ( ivectu - 1 + 6 * ( in - 1 ) + ii + 3 ) +&
                    knn * term(ii) * thetan
!
920              continue
!
            else
!
                do 930 ii = 1, 3
                    zr ( ivectu - 1 + 6 * ( in - 1 ) + ii ) = zr (&
                    ivectu - 1 + 6 * ( in - 1 ) + ii ) + knn * term(&
                    ii) * thetan
930              continue
!
            endif
!
        endif
!
900  continue
!
end subroutine

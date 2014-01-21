subroutine te0122(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/ppgan2.h"
#include "asterfort/tecach.h"
#include "asterfort/lteatt.h"
    character(len=16) :: option, nomte
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jerome.laverne at edf.fr
!......................................................................
!     FONCTION REALISEE:  CALCUL DES CHAMELEM AUX NOEUDS A PARTIR DES
!     VALEURS AUX POINTS DE GAUSS ( SIEF_ELNO ET VARI_ELNO )
!     ELEMENTS DE JOINT, JOINT_HYME, INTERFACE ET INTERFACE_S
!
! IN  OPTION : OPTION DE CALCUL
! IN  NOMTE  : NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: jgano, npg, nnos, nno, ndim, ndime, ipoids, ivf, idfde
    integer :: ichg, ichn, jtab(7), ncmp, ibid, i, j, in, ig, ntrou
    logical :: quadra, jhm, interf, jlin2d, jlin3d, jquad
    character(len=8) :: lielrf(10)
!     ------------------------------------------------------------------
!
    jlin2d = ((nomte.eq.'MFPLQU4' ).or. (nomte.eq.'MFAXQU4' ))
!
    jlin3d = ((nomte.eq.'MEFI_HEXA8' ) .or. (nomte.eq.'MEFI_PENTA6' ))
!
    jquad = ((nomte.eq.'MFPLQU8' ) .or. (nomte.eq.'MEFI_HEXA20' ) .or. (nomte.eq.'MEFI_PENTA15'))
!
    jhm = lteatt('TYPMOD2','EJ_HYME')
!
    interf = lteatt('TYPMOD2','INTERFAC')

!
    quadra = (jquad.or.jhm.or.interf)
!
! RAPPEL DES INTERPOLATIONS POUR L'ESPACE
! JOINT 2D (QUAD4)  EST BASE SUR LE QUAD4 A 2PG DONC NPG=2, NNO=4
! JOINT 3D (HEXA8)  EST BASE SUR LE QUAD4 A 4PG DONC NPG=4, NNO=4
! JOINT 3D (PENTA6) EST BASE SUR LE TRIA3 A 1PG DONC NPG=1, NNO=3
! JOINT_HYME 2D (QUAD8) EST BASE SUR LE SEG2 A 2PG DONC NPG=2, NNO=2
! JOINT_HYME 3D (HEXA20) EST BASE SUR LE QUAD4 A 4PG DONC NPG=4, NNO=4
! JOINT_HYME 3D (PENTA15) EST BASE SUR LE TRIA3 A 3PG DONC NPG=3, NNO=3
! INTERFACE_S 2D (QUAD8)   EST BASE SUR LE SEG2  A 2PG DONC NPG=2, NNO=2
! INTERFACE_S 3D (HEXA20)  EST BASE SUR LE QUAD4 A 4PG DONC NPG=4, NNO=4
! INTERFACE_S 3D (PENTA15) EST BASE SUR LE TRIA3 A 3PG DONC NPG=3, NNO=3
! INTERFACE   2D (QUAD8)   EST BASE SUR LE SEG2  A 3PG DONC NPG=3, NNO=2
! INTERFACE   3D (HEXA20)  EST BASE SUR LE QUAD4 A 9PG DONC NPG=9, NNO=4
! INTERFACE   3D (PENTA15) EST BASE SUR LE TRIA3 A 6PG DONC NPG=6, NNO=3
!
! REMARQUE POUR LES MODELISATIONS INTERFACE ON EFFECTUE EXACTEMENT LE
! MEME TRAITEMENT QUE POUR LES MODELISATIONS INTERFACE_S CE QUI VEUT
! DIRE QUE L'ON NE PREND EN COMPTE RESPECTIVEMENT QUE LES VALEURS DES
! 2, 4 ET 3 PREMIERS PG.
!
!     INFORMATIONS SUR L'ELEMENT DE REFERENCE
    if (quadra) then
        call elref2(nomte, 2, lielrf, ntrou)
        call elref4(lielrf(2), 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
!       DIMENSION ESPACE POUR LES JOINTS QUADRA, HYME OU INTERFACE
        ndime = ndim + 1
    else
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
!       DIMENSION DE L'ESPACE POUR LES JOINTS LINEAIRES
        if (jlin2d) ndime = ndim
        if (jlin3d) ndime = ndim + 1
    endif
!
!     INFORMATIONS SUR LES CHAMPS
    if (option .eq. 'SIEF_ELNO') then
!
        call jevech('PCONTRR', 'L', ichg)
        call jevech('PSIEFNOR', 'E', ichn)
!
        if (jhm .or. jquad) ncmp = 2*ndime-1
        if (interf) ncmp = 2*ndime
        if (.not.(quadra)) ncmp = ndime
!
    else if (option.eq.'VARI_ELNO') then
!
        call jevech('PVARIGR', 'L', ichg)
        call tecach('OOO', 'PVARINR', 'E', ibid, nval=7,&
                    itab=jtab)
        ncmp = max(jtab(6),1)*jtab(7)
        ichn = jtab(1)
!
    endif
!
! ############################################################
!     ELEMENTS QUADRATIQUE 2D ET 3D
! ############################################################
!
    if (quadra) then
!
        if (ndime .eq. 2) then
!
            do 10 i = 1, ncmp
!
                ig = ichg-1+i
                in = ichn-1+i
!           NOEUDS 1,4,8
                zr(in) = zr(ig+ncmp) + (zr(ig)-zr(ig+ncmp))*(1.d0- sqrt(3.d0))/2.d0
                zr(in+3*ncmp) = zr(ig+ncmp) + (zr(ig)-zr(ig+ncmp))*( 1.d0-sqrt(3.d0))/2.d0
                zr(in+7*ncmp) = zr(ig+ncmp) + (zr(ig)-zr(ig+ncmp))*( 1.d0-sqrt(3.d0))/2.d0
!           NOEUDS 2,3,6
                zr(in+ ncmp) = zr(ig+ncmp) + (zr(ig)-zr(ig+ncmp))*( 1.d0+sqrt(3.d0))/2.d0
                zr(in+2*ncmp) = zr(ig+ncmp) + (zr(ig)-zr(ig+ncmp))*( 1.d0+sqrt(3.d0))/2.d0
                zr(in+5*ncmp) = zr(ig+ncmp) + (zr(ig)-zr(ig+ncmp))*( 1.d0+sqrt(3.d0))/2.d0
!           NOEUDS 5,7
                zr(in+4*ncmp) = (zr(ig) + zr(ig+ncmp))/2.d0
                zr(in+6*ncmp) = (zr(ig) + zr(ig+ncmp))/2.d0
10          continue
!
        else
!
!         ON REMPLIT LES NOEUDS SOMMETS DE LA 1ERE FACE
!         (POUR L'HEXA20 : 1 2 3 4 , POUR LE PENTA15 : 1 2 3 )
            call ppgan2(jgano, 1, ncmp, zr(ichg), zr(ichn))
!
            do 18 i = 1, ncmp
!
                in = ichn-1+i
!
!           ON REMPLIT LES NOEUDS MILIEU DE LA 1ERE FACE
!           EN MOYENNANT LES VALEURS DES NOEUDS SOMMETS DE LA 1ERE FACE
!
                if (nno .eq. 4) then
!             (POUR L'HEXA20 : 9 10 11 12)
                    zr(in + 8*ncmp) = (zr(in ) + zr(in+ ncmp) )/2.d0
                    zr(in + 9*ncmp) = (zr(in+ ncmp) + zr(in+2*ncmp) )/ 2.d0
                    zr(in + 10*ncmp) = (zr(in+2*ncmp) + zr(in+3*ncmp) )/2.d0
                    zr(in + 11*ncmp) = (zr(in+3*ncmp) + zr(in ) )/ 2.d0
                else if (nno.eq.3) then
!             (POUR LE PENTA15 : 7 8 9)
                    zr(in + 6*ncmp) = (zr(in ) + zr(in+ ncmp) )/2.d0
                    zr(in + 7*ncmp) = (zr(in+ ncmp) + zr(in+2*ncmp) )/ 2.d0
                    zr(in + 8*ncmp) = (zr(in+2*ncmp) + zr(in ) )/2.d0
                endif
!
                do 19 j = 1, nno
!             ON REMPLIT LES NOEUDS SOMMETS DE LA 2ND FACE
!             (POUR L'HEXA20 : 5 6 7 8, POUR LE PENTA15 : 4 5 6)
!             AINSI QUE LES NOEUDS MILIEU INTERMEDIAIRES
!             (POUR L'HEXA20 : 13 14 15 16, POUR LE PENTA15 : 10 11 12)
!             A L'IDENTIQUE DES NOEUDS SOMMETS DE LA 1ERE FACE
                    zr(in+(j+ nno-1)*ncmp) = zr(in+(j-1)*ncmp)
                    zr(in+(j+3*nno-1)*ncmp) = zr(in+(j-1)*ncmp)
!             ON REMPLIT LES NOEUDS MILIEU DE LA DEUXIEME FACE
!             (POUR L'HEXA20 : 17 18 19 20, POUR LE PENTA15 : 13 14 15)
!             A L'IDENTIQUE DES NOEUDS MILIEU DE LA PREMIERE FACE
                    zr(in+(j+4*nno-1)*ncmp) = zr(in+(j+2*nno-1)*ncmp)
19              continue
!
18          continue
!
        endif
!
!#########################################
!     ELEMENTS DE JOINT LINEAIRES 2D ET 3D
!#########################################
    else
!
        if (ndime .eq. 2) then
!
!         REMARQUE : ICI LA POSITION DES 2 PG DE l'EJ SONT INVERSEES PAR
!         RAPPORT A LA POS HABITUELLE DES FAM D'EF 1D (SEG2 SEG3) A 2 PG
            do 11 i = 1, ncmp
                ig = ichg-1+i
                in = ichn-1+i
                zr(in) = zr(ig)+ (zr(ig+ncmp)-zr(ig))*(1.d0-sqrt(3.d0) )/2.d0
                zr(in+3*ncmp) = zr(ig)+ (zr(ig+ncmp)-zr(ig))*(1.d0- sqrt(3.d0))/2.d0
                zr(in+ncmp) = zr(ig)+ (zr(ig+ncmp)-zr(ig))*(1.d0+sqrt( 3.d0))/2.d0
                zr(in+2*ncmp) = zr(ig)+ (zr(ig+ncmp)-zr(ig))*(1.d0+ sqrt(3.d0))/2.d0
11          continue
!
        else
!
!         ON REMPLIT LES NOEUDS DE LA PREMIERE FACE
            call ppgan2(jgano, 1, ncmp, zr(ichg), zr(ichn))
!
!         ON REMPLIT LES NOEUD DE LA DEUXIEME FACE A L'IDENTIQUE
            do 20 i = 1, ncmp
                in = ichn-1+i
                do 30 j = 1, nno
                    zr(in+(j+nno-1)*ncmp) = zr(in+(j-1)*ncmp)
30              continue
20          continue
!
        endif
!
    endif
!
end subroutine

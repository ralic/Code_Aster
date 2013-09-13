subroutine te0123(option, nomte)
! ======================================================================
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
! ======================================================================
!  TE0123
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          EN 2D
!                          POUR ELEMENTS NON LOCAUX  A GRAD. DE SIG.
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/massup.h"
#include "asterfort/nmplgs.h"
#include "asterfort/rcangm.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/voiuti.h"
#include "blas/dcopy.h"
    character(len=16) :: option, nomte
!
    integer :: dlns
    integer :: nno, nnob, nnos, npg, imatuu, lgpg, lgpg1, lgpg2
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: ivfb, idfdeb, jgano
    integer :: icontm, ivarim
    integer :: iinstm, iinstp, idplgm, iddplg, icompo, icarcr
    integer :: ivectu, icontp, ivarip
    integer :: ivarix
    integer :: jtab(7), iadzi, iazk24, jcret, codret
    integer :: ndim, iret, ntrou, idim, i, vali(2)
!
    real(kind=8) :: trav1(3*8), angmas(7), bary(3)
    character(len=16) :: codvoi
    integer :: nvoima, nscoma, nbvois
    parameter(nvoima=12,nscoma=4)
    integer :: livois(1:nvoima), tyvois(1:nvoima), nbnovo(1:nvoima)
    integer :: nbsoco(1:nvoima), lisoco(1:nvoima, 1:nscoma, 1:2)
    integer :: numa
!
    integer :: icodr1
    character(len=8) :: typmod(2), lielrf(10), nomail
    character(len=16) :: phenom
!
!
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
    icontp=1
    ivarip=1
    imatuu=1
    ivectu=1
!
!
! - FONCTIONS DE FORME
    call elref2(nomte, 10, lielrf, ntrou)
    ASSERT(ntrou.ge.2)
!
    if (option(1:9) .eq. 'MASS_MECA') then
        call elref4(lielrf(1), 'MASS', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
    else
        call elref4(lielrf(1), 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
        call elref4(lielrf(2), 'RIGI', ndim, nnob, nnos,&
                    npg, ipoids, ivfb, idfdeb, jgano)
    endif
!
! - TYPE DE MODELISATION
    if (ndim .eq. 2 .and. lteatt(' ','C_PLAN','OUI')) then
        typmod(1) = 'C_PLAN  '
    else if (ndim.eq.2 .and. lteatt(' ','D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN  '
    else if (ndim .eq. 3) then
        typmod(1) = '3D'
    else
!       NOM D'ELEMENT ILLICITE
        ASSERT(ndim .eq. 3)
    endif
!
    typmod(2) = 'GRADSIGM'
    codret = 0
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    if (option(1:9) .eq. 'MASS_MECA') then
!---------------- CALCUL MATRICE DE MASSE ------------------------
!
        call jevech('PMATUUR', 'E', imatuu)
!
        if (ndim .eq. 2) then
! - 2 DEPLACEMENTS + 4 DEF
            dlns = 6
        else if (ndim.eq.3) then
! - 3 DEPLACEMENTS + 6 DEF
            dlns = 9
        else
            ASSERT(ndim .eq. 3)
        endif
!
        call massup(option, ndim, dlns, nno, nnos,&
                    zi(imate), phenom, npg, ipoids, idfde,&
                    zr(igeom), zr(ivf), imatuu, icodr1, igeom,&
                    ivf)
!
!--------------- FIN CALCUL MATRICE DE MASSE -----------------------
    else
!---------------- CALCUL OPTION DE RIGIDITE ------------------------
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PDEPLMR', 'L', idplgm)
        call jevech('PDEPLPR', 'L', iddplg)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCARCRI', 'L', icarcr)
!
! - ON INTERDIT UNE LOI DIFFERENTE DE ENDO_HETEROGENE
        if (zk16(icompo) .ne. 'ENDO_HETEROGENE') then
            call utmess('F', 'COMPOR2_13')
        endif
!
! - ON VERIFIE QUE PVARIMR ET PVARIPR ONT LE MEME NOMBRE DE V.I. :
!
        call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                    iret)
        ASSERT(jtab(1).eq.ivarim)
        lgpg1 = max(jtab(6),1)*jtab(7)
!
        if ((option.eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
            call tecach('OON', 'PVARIPR', 'E', 7, jtab,&
                        iret)
            lgpg2 = max(jtab(6),1)*jtab(7)
            if (lgpg1 .ne. lgpg2) then
                call tecael(iadzi, iazk24)
                nomail = zk24(iazk24-1+3) (1:8)
                vali(1)=lgpg1
                vali(2)=lgpg2
                call utmess('F', 'CALCULEL6_64', sk=nomail, ni=2, vali=vali)
            endif
        endif
        lgpg = lgpg1
!
! --- ORIENTATION DU MASSIF
!     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
!
        bary(1) = 0.d0
        bary(2) = 0.d0
        bary(3) = 0.d0
        do 150 i = 1, nno
            do 140 idim = 1, ndim
                bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/ nno
140          continue
150      continue
        call rcangm(ndim, bary, angmas)
!
! - VARIABLES DE COMMANDE
!
        call jevech('PINSTMR', 'L', iinstm)
        call jevech('PINSTPR', 'L', iinstp)
!
! PARAMETRES EN SORTIE
!
        if (option(1:14) .eq. 'RIGI_MECA_TANG' .or. option(1:14) .eq. 'RIGI_MECA_ELAS' .or.&
            option(1:9) .eq. 'FULL_MECA') then
            call jevech('PMATUNS', 'E', imatuu)
        endif
!
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PVECTUR', 'E', ivectu)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
!
!      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
            call jevech('PVARIMP', 'L', ivarix)
            call dcopy(npg*lgpg, zr(ivarix), 1, zr(ivarip), 1)
        endif
!
! - HYPO-ELASTICITE
!
        call tecael(iadzi, iazk24)
        numa=zi(iadzi-1+1)
        codvoi='A2'
!
        call voiuti(numa, codvoi, nvoima, nscoma, jrepe,&
                    jptvoi, jelvoi, nbvois, livois, tyvois,&
                    nbnovo, nbsoco, lisoco)
!
        if (zk16(icompo+2) .ne. 'PETIT') then
            call utmess('F', 'ELEMENTS3_16', sk=zk16(icompo+2))
        endif
!
!
        call nmplgs(ndim, nno, zr(ivf), idfde, nnob,&
                    zr(ivfb), idfdeb, npg, ipoids, zr(igeom),&
                    typmod, option, zi(imate), zk16(icompo), zr(icarcr),&
                    zr(iinstm), zr(iinstp), angmas, zr(idplgm), zr(iddplg),&
                    zr(icontm), lgpg, zr(ivarim), zr(icontp), zr(ivarip),&
                    zr(imatuu), zr(ivectu), codret, trav1, livois,&
                    nbvois, numa, lisoco, nbsoco)
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
            call jevech('PCODRET', 'E', jcret)
            zi(jcret) = codret
        endif
!---------------- FIN CALCUL OPTION DE RIGIDITE ------------------------
    endif
end subroutine

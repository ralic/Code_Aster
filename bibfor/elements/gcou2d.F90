subroutine gcou2d(base, resu, noma, nomno, noeud,&
                  coor, rinf, rsup, module, ldirec,&
                  dir)
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8prem.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    real(kind=8) :: rinf, rsup, module, dir(3), coor(*)
    character(len=1) :: base
    character(len=8) :: noma, noeud
    character(len=24) :: resu, nomno
    aster_logical :: ldirec
!
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! FONCTION REALISEE:
!
!     OPTION COURONNE EN 2D
!     ---------------------
!
! 1.  POUR LE NOEUD DU FOND DE FISSURE ON RECUPERE
!     LE TRIPLET ( MODULE(THETA), RINF, RSUP )
!
! 2.  ENSUITE ON CALCUL THETA SUR TOUT LES NOEUDS DU MAILLAGE
!
!     ------------------------------------------------------------------
! ENTREE:
!        RESU   : NOM DU CONCEPT DE TYPE CHAM_NO
!        NOMA   : NOM DU MAILLAGE
!        NOMNO  : NOM DE L'OBJET CONTENANT LES NOEUDS DU MAILLAGE
!        NOEUD  : NOM DU NOEUD DU FOND DE FISSURE
!        COOR   : COORDONNEES DES NOEUDS
!        RINF   : RAYON INFERIEURE DE LA COURONNE
!        RSUP   : RAYON SUPERIEURE DE LA COURONNE
!        MODULE : MODULE(THETA)
!        DIR    : DIRECTION DU CHAMPS THETA
!        LDIREC : INDIQUE SI LA DIRECTION A ETE DONNEE
!
! SORTIE:
!        DIR    : DIRECTION DU CHAMPS THETA NORMALISEE
!     ------------------------------------------------------------------
!
!
    integer :: itheta, i, irefe, idesc, num, nbel, numa
    integer :: nec, ibid, numfon, n1, n2, ndim, jgtl, estbf
    parameter     (ndim=2)
    real(kind=8) :: xm, ym, xi, yi, eps, d, norme, alpha, valx, valy
    character(len=8) :: k8b, fiss
    character(len=16) :: k16b, nomcmd
    character(len=19) :: grlt, chgrs
    character(len=24) :: chamno
    real(kind=8), pointer :: fondfiss(:) => null()
    real(kind=8), pointer :: cnsv(:) => null()
    real(kind=8), pointer :: vbasfd(:) => null()
    aster_logical :: estfem
!     ------------------------------------------------------------------
!
    call jemarq()
    eps = 1.d-06
!
    call getres(k8b, k16b, nomcmd)
    n1=1
    n2=0
!
    if (nomcmd .eq. 'CALC_G') then
!       CAS CLASSIQUE (N1 NON NUL) OU CAS X-FEM (N2 NON NUL)
        call getvid('THETA', 'FOND_FISS', iocc=1, scal=k8b, nbret=n1)
        call getvid('THETA', 'FISSURE', iocc=1, scal=fiss, nbret=n2)
    endif

!   TEST DU TYPE DE FISSURE ET RECUPERATION DU NUMERO DE NOEUD DU FOND DE FISSURE FEM
    estfem=.true.
    if (n1 .ne. 0) then
        estfem=.true.
        call jenonu(jexnom(nomno, noeud), num)
    else if (n2 .ne. 0) then
        estfem=.false.
        num = 0
    end if
!
!     DANS LE CAS X-FEM, SI LA DIRECTION A ETE DONNEE, ON LA GARDE FIXE
!     SI ELLE N'A PAS ETE DONNEE, ON PREND UNE DIRECTION VARIABLE QUI
!     VAUT LE GRADIENT DE LA LEVEL SET TANGENTE

    ! On verifie l'existence de basefond
    call jeexin(k8b//'.BASEFOND', estbf)

    if (ldirec) then
!     --- LA DIRECTION DE THETA EST DONNEE, ON LA NORME ---
        norme = 0.d0
        do i = 1, 2
            norme = norme + dir(i)*dir(i)
        end do
        norme = sqrt(norme)
        dir(1) = dir(1)/norme
        dir(2) = dir(2)/norme
        if (estbf .ne. 0) then
            ! Si basefond existe alors il est risque pour l'util.
            ! de renseigner la direction qui peut être fausse.
            call utmess('A', 'RUPTURE0_20')
        end if
!     --- LA DIRECTION DE THETA N'EST DONNEE, ON LA RECUPERE
!         DE BASEFOND CALCULE DANS DEFI_FOND_FISS. ---
    else if (estfem) then
        if (estbf .eq. 0) then
           ! basefond n'existe pas, la fissure est probablement
           ! definie avec calc_theta sans passer par defi_fond_fiss. Il FAUT
           ! alors renseigner DIRECTION.
            call utmess('F', 'RUPTURE0_58')
        end if

        call jeveuo(k8b//'.BASEFOND', 'L', vr=vbasfd)
        if (size(vbasfd).gt.4) then
            ! le front ne doit contenir qu'un noeud, donc 4 composantes dans basefond
            call utmess('F', 'RUPTURE0_33')
        end if
        norme = sqrt(vbasfd(3)**2+vbasfd(4)**2)
!       Basefond contient 4*1 composantes (XN YN XP YP)i
!       Avec N pour direction normale et P pour direction plan.
        dir(1) = vbasfd(3)/norme
        dir(2) = vbasfd(4)/norme
        dir(3) = 0.d0
    endif
!
!  .DESC
    chamno = resu(1:19)//'.DESC'
    call dismoi('NB_EC', 'DEPL_R', 'GRANDEUR', repi=nec)
    call wkvect(chamno, base//' V I', 2+nec, idesc)
!
    call jeecra(chamno, 'DOCU', cval='CHNO')
    call jenonu(jexnom('&CATA.GD.NOMGD', 'DEPL_R'), numa)
    zi(idesc+1-1) = numa
    zi(idesc+2-1) = -2
    zi(idesc+3-1) = 6
!
!  .REFE
    chamno = resu(1:19)//'.REFE'
    call wkvect(chamno, base//' V K24', 4, irefe)
    zk24(irefe+1-1) = noma//'                '
!
!  .VALE
    chamno = resu(1:19)//'.VALE'
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbel)
    call wkvect(chamno, base//' V R', 2*nbel, itheta)
!
!     --- CALCUL DE THETA ---
!
!     CAS CLASSIQUE
    if (estfem) then
!       NOEUD DU FOND DE FISSURE
        zr(itheta + (num-1)*2 + 1 - 1) = module*dir(1)
        zr(itheta + (num-1)*2 + 2 - 1) = module*dir(2)
        xi = coor((num-1)*3+1)
        yi = coor((num-1)*3+2)
!     CAS X-FEM
    else if (.not. estfem) then
        call getvis('THETA', 'NUME_FOND', iocc=1, scal=numfon, nbret=ibid)
        call jeveuo(fiss//'.FONDFISS', 'L', vr=fondfiss)
        xi = fondfiss(4*(numfon-1)+1)
        yi = fondfiss(4*(numfon-1)+2)
        if (.not.ldirec) then
            call utmess('I', 'XFEM_10')
!         RÉCUPÉRATION DU GRADIENT DE LST
            grlt = fiss//'.GRLTNO'
            chgrs = '&&GCOU2D.GRLT'
            call cnocns(grlt, 'V', chgrs)
            call jeveuo(chgrs//'.CNSV', 'L', vr=cnsv)
            call jeveuo(chgrs//'.CNSL', 'L', jgtl)
        endif
    endif
!
!
! BOUCLE SUR LES AUTRES NOEUDS COURANTS DU MAILLAGE
!
    do i = 1, nbel
        if (i .ne. num) then
            xm = coor((i-1)*3+1)
            ym = coor((i-1)*3+2)
            d = sqrt((xi-xm)*(xi-xm)+(yi-ym)*(yi-ym))
            alpha = (d-rinf)/(rsup-rinf)
            if ((.not.ldirec) .and. (.not. estfem)) then
!           LE GRANDIENT EST DÉFINI
                if (zl(jgtl-1+ndim*(i-1)+1)) then
                    dir(1) = cnsv(ndim*(i-1)+1)
                    dir(2) = cnsv(ndim*(i-1)+2)
                    norme = sqrt(dir(1)**2+dir(2)**2)
                else
!           LE GRANDIENT N'EST PAS DÉFINI
                    dir(1) = 0.d0
                    dir(2) = 0.d0
                    norme = 1.d0
                endif
!           IL SE PEUT QUE EN CERTAINS POINTS, LE GRADIENT SOIT NUL
!           CES POINTS SONT NORMALEMENT HORS COURONNE THETA
                if (norme .le. r8prem()*1.d04) then
                    if ((abs(alpha-1).gt.eps) .and. ((alpha-1).le.0)) then
                        call jenuno(jexnum(noma//'.NOMNOE', i), k8b)
                        call utmess('F', 'XFEM_12', sk=k8b)
                    endif
                    norme = 1.d0
                endif
                dir(1) = dir(1)/norme
                dir(2) = dir(2)/norme
            endif
            valx = module*dir(1)
            valy = module*dir(2)
            if ((abs(alpha).le.eps) .or. (alpha.lt.0)) then
                zr(itheta+(i-1)*2+1-1) = valx
                zr(itheta+(i-1)*2+2-1) = valy
            else if ((abs(alpha-1).le.eps).or.((alpha-1).gt.0)) then
                zr(itheta+(i-1)*2+1-1) = 0.d0
                zr(itheta+(i-1)*2+2-1) = 0.d0
            else
                zr(itheta+(i-1)*2+1-1) = (1-alpha)*valx
                zr(itheta+(i-1)*2+2-1) = (1-alpha)*valy
            endif
        endif
    end do
!
    if (.not.ldirec) call detrsd('CHAM_NO_S', chgrs)
!
    call jedema()
end subroutine

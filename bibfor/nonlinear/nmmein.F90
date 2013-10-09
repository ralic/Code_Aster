subroutine nmmein(fiss, noma, nno, numnod, liscmp,&
                  nbno, gro1, gro2, ndim, compo)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmaret.h"
#include "asterfort/wkvect.h"
#include "asterfort/xlagsp.h"
    character(len=8) :: fiss, noma
    integer :: nno, nbno, ndim
    character(len=24) :: gro1, gro2, numnod, liscmp
    character(len=8) :: compo
!
! ----------------------------------------------------------------------
!
! INITIALISATION DU PILOTAGE DDL_IMPO OU LONG_ARC - FORMULATION XFEM
!
! RENVOIE L'ENSEMBLE DES ARETES PILOTEES ET INITIALISE LES COMPOSANTES
! PILOTEES DANS LE CAS DTAN OU DNOR
!
! ----------------------------------------------------------------------
!
!
! IN  FISS   : SD FISSURE
! IN  NOMA   : NOM DU MAILLAGE
! IN NNO     : NOMBRE DE NOEUDS ENTRES PAR L UTILISATEUR
! IN  NUMNOD : LISTE DES NOEUDS ENTREE PAR L UTILISATEUR
! IN/OUT LISCMP : LISTE DES COMPOSANTES PILOTEES
! OUT NBNO   : NOMBRE D ARETES FINALEMENT PILOTEES
! OUT GRO1   : LISTE DES NOEUDS EXTREMITE 1 DES ARETES PILOTEES
! OUT GRO2   : LISTE DES NOEUDS EXTREMITE 2 DES ARETES PILOTEES
! OUT NDIM   : DIMENSION DE L ESPACE
! OUT COMPO  : NOM DE LA COMPOSANTE UTILISATEUR
!
!
!
!
    integer :: alglag, i, nddl
    character(len=8) :: nomap, nomo
    character(len=19) :: nlisco, nliseq, nlisrl, nbasco
    integer :: jlicmp, iadrma
    integer :: nbarvi, ibid
!
! ----------------------------------------------------------------------
!
    call jeveuo(liscmp, 'E', jlicmp)
    call jelira(liscmp, 'LONMAX', ival=nddl)
    call getvid(' ', 'MODELE', scal=nomo, nbret=ibid)
    call jeveuo(nomo(1:8)//'.MODELE    .LGRF', 'L', iadrma)
    nomap = zk8(iadrma)
    call dismoi('DIM_GEOM', nomap, 'MAILLAGE', repi=ndim)
!
    nliseq = '&&NMMEIN.LISEQ'
    nlisrl = '&&NMMEIN.LISRL'
    nlisco = '&&NMMEIN.LISCO'
    nbasco = '&&NMMEIN.BASCO'
    alglag = 2
    call xlagsp(noma, nomo, fiss, alglag, ndim,&
                nliseq)
!
!
    call jelira(nliseq, 'LONMAX', ival=nbarvi)
    nbarvi=nbarvi/2
    call nmaret(nbarvi, nno, ndim, nliseq, nbno,&
                numnod, gro1, gro2)
    do 1 i = 1, nddl
        compo = zk8(jlicmp-1+i)
        if (compo .eq. 'DX') zk8(jlicmp-1+i)='H1X'
        if (compo .eq. 'DY') zk8(jlicmp-1+i)='H1Y'
        if (compo .eq. 'DZ') zk8(jlicmp-1+i)='H1Z'
        if (compo(1:4) .eq. 'DTAN' .or. compo .eq. 'DNOR') then
            call jedetr(liscmp)
            call wkvect(liscmp, 'V V K8', ndim, jlicmp)
            zk8(jlicmp)='H1X'
            zk8(jlicmp+1)='H1Y'
            if (ndim .eq. 3) zk8(jlicmp+2)='H1Z'
            goto 2
        endif
  1 end do
  2 continue
    call jedetr(nliseq)
    call jedetr(nlisrl)
    call jedetr(nlisco)
    call jedetr(nbasco)
end subroutine

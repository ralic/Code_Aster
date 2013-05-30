subroutine ccchut(resuin, resuou, lisord, nbordr)
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
    implicit none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/ccchuc.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: nbordr
    character(len=8) :: resuou, resuin
    character(len=19) :: lisord
! person_in_charge: mathieu.courtois at edf.fr
! ----------------------------------------------------------------------
!  CALC_CHAMP - TRAITEMENT DE CHAM_UTIL
!  -    -                     --   --
! ----------------------------------------------------------------------
! IN  :
!   RESUIN K8   NOM DE LA SD IN
!   RESUOU K8   NOM DE LA SD OUT
!   LISORD K19  NOM DE LA LISTE DES NUMEROS D'ORDRE
!   NBORDR I    NOMBRE DE NUMEROS D'ORDRE
! ----------------------------------------------------------------------
    character(len=9) :: mcfact
    parameter   (mcfact='CHAM_UTIL')
    character(len=19) :: lform
    parameter   (lform='&&CCHUT.FORMULE    ')
!
    integer :: ifm, niv, ioc, nuti, nf, nc, ibid
    integer :: jform, nchout
    character(len=8) :: k8b
    character(len=16) :: chin, crit
    integer :: iarg
!     ----- FIN  DECLARATIONS ------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    call getfac(mcfact, nuti)
    if (nuti .eq. 0) then
        goto 9999
    endif
!
!     BOUCLE SUR LES OCCURRENCES DE CHAM_UTIL
    do 10 ioc = 1, nuti
        call getvtx(mcfact, 'NOM_CHAM', ioc, iarg, 1,&
                    chin, ibid)
        call getvis(mcfact, 'NUME_CHAM_RESU', ioc, iarg, 1,&
                    nchout, ibid)
        call assert(nchout.ge.1 .and. nchout.le.20)
!       CRITERE OU FORMULE ?
        crit = ' '
        call getvid(mcfact, 'FORMULE', ioc, iarg, 0,&
                    k8b, nf)
        if (nf .eq. 0) then
            call getvtx(mcfact, 'CRITERE', ioc, iarg, 1,&
                        crit, nc)
            jform = 1
        else
            nf = -nf
            call wkvect(lform, 'V V K8', nf, jform)
            call getvid(mcfact, 'FORMULE', ioc, iarg, nf,&
                        zk8(jform), ibid)
        endif
        call ccchuc(resuin, resuou, chin, nchout, crit,&
                    nf, zk8(jform), lisord, nbordr)
        call jedetr(lform)
10  end do
!
9999  continue
    call jedema()
!
end subroutine

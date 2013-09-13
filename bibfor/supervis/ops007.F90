subroutine ops007()
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jvinfo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/wkvect.h"
!
! ----------------------------------------------------------------------
!
!     OPERATEUR DESTRUCTION DE CONCEPT ET D'OBJETS JEVEUX
!
! ----------------------------------------------------------------------
!
    character(len=1) :: klas
    character(len=32) :: kch
    integer :: ifm, niv
    integer :: l
    integer :: ibid
    integer :: iocc, nbocc
    integer :: ipos, npos, jlpos
    integer :: icon, ncon, jlcon
    integer :: iobj, nobj, jlobj
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
    if (niv .gt. 1) ibid = jvinfo('AFFECT', niv)
!
! --- DESTRUCTION DES CONCEPTS
!
    call getfac('CONCEPT', nbocc)
    do 10 iocc = 1, nbocc
        call getvid('CONCEPT', 'NOM', iocc=iocc, nbval=0, nbret=ncon)
        ncon = -ncon
        if (ncon .gt. 0) then
            call wkvect('&&OPS007.LISTE_CO', 'V V K8', ncon, jlcon)
            call getvid('CONCEPT', 'NOM', iocc=iocc, nbval=ncon, vect=zk8(jlcon),&
                        nbret=ibid)
            do 15 icon = 1, ncon
                call jedetc('G', zk8(jlcon-1+icon), 1)
15          continue
            call jedetr('&&OPS007.LISTE_CO')
        endif
10  end do
!
! --- DESTRUCTION DES OBJETS
!
    call getfac('OBJET', nbocc)
    do 20 iocc = 1, nbocc
        call getvtx('OBJET', 'CLASSE', iocc=iocc, scal=klas, nbret=nobj)
        call getvtx('OBJET', 'CHAINE', iocc=iocc, nbval=0, nbret=nobj)
        nobj = -nobj
        call wkvect('&&OPS007.NOMOBJ', 'V V K24', nobj, jlobj)
        call getvtx('OBJET', 'CHAINE', iocc=iocc, nbval=nobj, vect=zk24(jlobj),&
                    nbret=ibid)
        call getvis('OBJET', 'POSITION', iocc=iocc, nbval=0, nbret=npos)
        npos = -npos
        if (npos .lt. nobj) then
            call wkvect('&&OPS007.NIPOSI', 'V V IS', nobj, jlpos)
            do 21 ipos = npos+1, nobj
                zi(jlpos+ipos-1) = 1
21          continue
        else
            call wkvect('&&OPS007.NIPOSI', 'V V IS', npos, jlpos)
        endif
        call getvis('OBJET', 'POSITION', iocc=iocc, nbval=npos, vect=zi(jlpos),&
                    nbret=ibid)
        do 22 iobj = 1, nobj
            kch = zk24(jlobj+iobj-1)
            l = lxlgut(kch)
            if (l .gt. 0) then
                call jedetc(klas, kch(1:l), zi(jlpos+iobj-1))
            endif
22      continue
        call jedetr('&&OPS007.NOMOBJ')
        call jedetr('&&OPS007.NIPOSI')
20  end do
    if (niv .gt. 1) ibid=jvinfo('AFFECT', 0)
    call jedema()
end subroutine

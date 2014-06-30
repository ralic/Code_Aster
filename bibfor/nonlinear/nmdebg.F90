subroutine nmdebg(typobz, nomobz, ifm)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/tstobj.h"
#include "asterfort/utimsd.h"
    character(len=*) :: nomobz
    character(len=*) :: typobz
    integer :: ifm, niv
!
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! IMPRIME LE CONTENU D'UN OBJET POUR DEBUG
!
! ----------------------------------------------------------------------
!
!
! IN  TYPOBJ : TYPE DE L'OBJET (VECT/MATR)
! IN  NOMOBJ : NOM DE L'OBJET
! IN  IFM    : UNITE D'IMPRESSION
!
!
!
!
    character(len=24) :: nomobj
    character(len=4) :: typobj
    character(len=3) :: type
    real(kind=8) :: sommr
    integer :: resume, sommi, lonmax
    integer :: iret, ibid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nomobj = nomobz
    typobj = typobz
!
    call infdbg('MECA_NON_LINE', ibid, niv)
    niv = 2
!
! --- IMPRESSION
!
    if (typobj .eq. 'VECT') then
        call tstobj(nomobj(1:19)//'.VALE', 'OUI', resume, sommi, sommr,&
                    ibid, lonmax, type, iret, ibid)
        if ((type.eq.'R') .and. (iret.eq.0)) then
            if (niv .ge. 2) then
                write (ifm,1003) nomobj(1:19),lonmax,sommr
            endif
        endif
    else if (typobj.eq.'CHEL') then
        call tstobj(nomobj(1:19)//'.CELV', 'OUI', resume, sommi, sommr,&
                    ibid, lonmax, type, iret, ibid)
        if ((type.eq.'R') .and. (iret.eq.0)) then
            if (niv .ge. 2) then
                write (ifm,1003) nomobj(1:19),lonmax,sommr
            endif
        endif
    else if (typobj.eq.'MATA') then
        call tstobj(nomobj(1:19)//'.VALM', 'OUI', resume, sommi, sommr,&
                    ibid, lonmax, type, iret, ibid)
        if ((type.eq.'R') .and. (iret.eq.0)) then
            if (niv .ge. 2) then
                write (ifm,1003) nomobj(1:19),lonmax,sommr
            endif
        endif
    else
        call utimsd(ifm, -1, .true._1, .true._1, nomobj(1:24),&
                    1, ' ')
    endif
!
    1003 format (' <MECANONLINE>        ',a19,' | LONMAX=',i12,&
     &        ' | SOMMR=',e30.21)
!
    call jedema()
!
end subroutine

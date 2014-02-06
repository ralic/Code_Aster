subroutine utesto(nomobj, type, tbtxt, refi, refr,&
                  epsi, crit, ific, llab, ssigne)
    implicit none
#include "asterfort/jeexin.h"
#include "asterfort/tstobj.h"
#include "asterfort/utites.h"
#include "asterfort/utmess.h"
    character(len=24) :: nomobj
    real(kind=8) :: refr, epsi
    character(len=*) :: crit, type, ssigne
    character(len=16) :: tbtxt(2)
    integer :: refi, ific
    logical :: llab
! ----------------------------------------------------------------------
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
!     ENTREES:
!        NOMOBJ : NOM DE L'OBJET JEVEUX QUE L'ON VEUT TESTER
!        TYPE   : TYPE DE VALEUR A TESTER :
!                  /'RESUME' : ENTIER QUI RESUME L'OBJET
!                  /'S_I'    : SOMME ENTIERE DE L'OBJET
!                  /'S_R'    : SOMME REELLE DE L'OBJET
!        TBTXT  : (1) : REFERENCE
!                 (2) : LEGENDE
!        REFI   : VALEUR ENTIERE ATTENDUE POUR L'OBJET
!        REFR   : VALEUR REELLE ATTENDUE POUR L'OBJET
!        CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
!        EPSI   : PRECISION ESPEREE
!        IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
!        LLAB   : FLAG D IMPRESSION DES LABELS
!     SORTIES:
!      LISTING ...
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
    character(len=3) :: tysc
    complex(kind=8) :: cbid
    real(kind=8) :: rbid, sommr
    integer :: ibid, resume, sommi, lonuti, lonmax, ni, iret, iret2
!
    call tstobj(nomobj, 'NON', resume, sommi, sommr,&
                lonuti, lonmax, tysc, iret, ni)
!
    rbid = 0.d0
    cbid = dcmplx(0.d0, 0.d0)
    ibid = 0
    if (iret .eq. 0) then
        if (type .eq. 'RESUME') then
            call utites(tbtxt(1), tbtxt(2), 'I', 1, [refi],&
                        [rbid], [cbid], resume, rbid, cbid,&
                        epsi, crit, ific, llab, ssigne)
        else if (type.eq.'I') then
            call utites(tbtxt(1), tbtxt(2), 'I', 1, [refi],&
                        [rbid], [cbid], sommi, rbid, cbid,&
                        epsi, crit, ific, llab, ssigne)
        else if (type.eq.'R') then
            call utites(tbtxt(1), tbtxt(2), 'R', 1, [ibid],&
                        [refr], [cbid], ibid, sommr, cbid,&
                        epsi, crit, ific, llab, ssigne)
        endif
    else
        call jeexin(nomobj, iret2)
        if (iret2 .le. 0) then
            call utmess('F', 'CALCULEL6_86', sk=nomobj)
        else
            call utmess('F', 'CALCULEL6_87', sk=nomobj)
        endif
    endif
!
end subroutine

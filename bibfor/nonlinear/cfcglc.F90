subroutine cfcglc(ndim, neq, nesmax, resoco, iliai,&
                  jdecal, nbddl, jdepc, japddl, japcof,&
                  glis)
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
    implicit     none
#include "jeveux.h"
!
#include "asterfort/caladu.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: iliai, jdecal
    integer :: ndim, neq, nesmax, nbddl
    integer :: japcof, japddl, jdepc
    real(kind=8) :: glis
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION)
!
! CALCUL DU GLISSEMENT - CAS DUALISE: JEU CORRIGE
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  ILIAI  : INDICE DE LA LIAISON COURANTE
! OUT GLIS   : GLISSEMENT TANGENT
!
!
!
!
    character(len=24) :: jeuite, jeux
    integer :: jjeuit, jjeux
    real(kind=8) :: jexold, jeyold, jexinc, jeyinc, jexnew, jeynew
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    jeuite = resoco(1:14)//'.JEUITE'
    jeux = resoco(1:14)//'.JEUX'
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(jeux, 'L', jjeux)
!
! --- JEUX TANGENTS AVANT ITERATION DE NEWTON
!
    jexold = zr(jjeuit+3*(iliai-1)+2-1)
    jeyold = zr(jjeuit+3*(iliai-1)+3-1)
!
! --- INCR. CORRIGE DES JEUX TANGENTS
!
    jexinc = 0.d0
    jeyinc = 0.d0
    call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), zr(jdepc),&
                jexinc)
    if (ndim .eq. 3) then
        call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+ jdecal), zr(jdepc),&
                    jeyinc)
    endif
!
! --- CALCUL DES JEUX TANGENTS
!
    jexnew = jexold - jexinc
    jeynew = jeyold - jeyinc
    glis = sqrt(jexnew**2+jeynew**2)
!
    call jedema()
!
end subroutine

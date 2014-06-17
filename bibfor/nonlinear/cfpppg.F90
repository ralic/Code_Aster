subroutine cfpppg(resoco, ndim, neq, nesmax, nbliac,&
                  glimin, glimax)
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
!
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/caladu.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    real(kind=8) :: glimin, glimax
    integer :: nbliac, neq, nesmax, ndim
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! DETERMINATION DU PLUS GRAND ET DU PLUS PETIT GLISSEMENT TANGENT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! OUT GLIMIN : GLISSEMENT MINIMUM
! OUT GLIMAX : GLISSEMENT MAXIMUM
!
!
!
!
    integer :: iliai, iliac
    real(kind=8) :: jexold, jeyold, jexnew, jeynew, jexinc, jeyinc
    character(len=24) :: apddl, appoin, apcofr
    integer :: japddl, japptr, japcof
    character(len=19) :: liac
    integer :: jliac
    character(len=24) :: jeuite
    integer :: jjeuit
    character(len=19) :: deplc
    integer :: nbddl, jdecal
    real(kind=8) :: glis
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    glimax = 0.d0
    glimin = r8maem()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcofr, 'L', japcof)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    jeuite = resoco(1:14)//'.JEUITE'
    call jeveuo(jeuite, 'L', jjeuit)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DEPLC : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT
! ---         DU PAS DE TEMPS AVEC CORRECTION DU CONTACT
!
    deplc = resoco(1:14)//'.DEPC'
    call jeveuo(deplc (1:19)//'.VALE', 'L', vr=vale)
!
! --- CALCUL DES GLISSEMENTS
!
    do 10 iliac = 1, nbliac
!
! ----- REPERAGE DE LA LIAISON
!
        iliai = zi(jliac-1+iliac)
        jdecal = zi(japptr+iliai-1)
        nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
!
! ----- JEUX TANGENTS AVANT ITERATION DE NEWTON
!
        jexold = zr(jjeuit+3*(iliai-1)+2-1)
        jeyold = zr(jjeuit+3*(iliai-1)+3-1)
!
! ----- INCREMENT DES JEUX TANGENTS
!
        jexinc = 0.d0
        jeyinc = 0.d0
        call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), vale,&
                    jexinc)
        if (ndim .eq. 3) then
            call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal), vale,&
                        jeyinc)
        endif
!
! ----- CALCUL DES JEUX TANGENTS
!
        jexnew = jexold - jexinc
        jeynew = jeyold - jeyinc
        glis = sqrt(jexnew**2+jeynew**2)
        glimax = max(glimax,glis)
        glimin = min(glimin,glis)
10  end do
    glimin = max(glimin,r8prem())
!
    call jedema()
!
end subroutine

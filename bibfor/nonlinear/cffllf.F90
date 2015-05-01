subroutine cffllf(resoco, ndim, neq, nesmax, nbliac,&
                  nbliai, glitol, glimin, glimax)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit     none
#include "jeveux.h"
#include "asterfort/cfcglc.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: nbliai, nbliac, ndim, nesmax, neq
    real(kind=8) :: glitol, glimin, glimax
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - PENALISATION)
!
! CALCUL DES COEFFICIENTS DE LAGRANGE MU POUR LE FROTTEMENT
!
! CAS INCREMENTAL
!
! ----------------------------------------------------------------------
!
! --- ON REMPLIT ZR(JMU-1+3*NBLIAI+LLIAC) PAR LA RACINE DU
! --- RAPPORT ENTRE KPG ET NORME DE MUG. IL DOIT RESTER
! --- INFERIEUR OU EGAL A RACINE DE E_T. SI MUG EST TROP PETIT
! --- ON INTRODUIT UNE VALEUR QUI CONSERVE LE CONDITIONNEMENT
! --- DE LA MATRICE DE FROTTEMENT
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  NBLIAI : NOMBRE DE LIAISONS
! IN  GLIMIN : GLISSEMENT MINIMUM
! IN  GLIMAX : GLISSEMENT MAXIMUM
! IN  GLITOL : TOELRANCE POUR DETECTER UN GLISSEMENT NUL
!
!
!
!
    real(kind=8) :: coefpt, coefff, lambdc, lambdf
    real(kind=8) :: glis
    integer :: iliai, iliac
    character(len=19) :: mu, liac
    integer :: jmu, jliac
    character(len=24) :: tacfin
    integer :: jtacf
    character(len=24) :: apddl, appoin, apcofr
    integer :: japddl, japptr, japcof
    integer :: ztacf
    integer :: nbddl, jdecal
    character(len=19) :: deplc
    integer :: jdepc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    tacfin = resoco(1:14)//'.TACFIN'
    mu = resoco(1:14)//'.MU'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcofr, 'L', japcof)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(tacfin, 'L', jtacf)
    call jeveuo(mu, 'E', jmu)
    ztacf = cfmmvd('ZTACF')
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DEPLC : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT
! ---         DU PAS DE TEMPS AVEC CORRECTION DU CONTACT
!
    deplc = resoco(1:14)//'.DEPC'
    call jeveuo(deplc (1:19)//'.VALE', 'L', jdepc)
!
! --- CALCUL DES COEFFICIENTS DE LAGRANGE MU
!
    do 100 iliac = 1, nbliac
!
! ----- REPERAGE DE LA LIAISON
!
        iliai = zi(jliac-1+iliac)
        jdecal = zi(japptr+iliai-1)
        nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
!
! ----- CALCUL DU JEU TANGENT CORRIGE
!
        call cfcglc(ndim, neq, nesmax, resoco, iliai,&
                    jdecal, nbddl, jdepc, japddl, japcof,&
                    glis)
!
! ----- COEFFICIENTS
!
        coefff = zr(jtacf+ztacf*(iliai-1)+1-1)
        coefpt = zr(jtacf+ztacf*(iliai-1)+3-1)
!
! ----- LAMBDA DE CONTACT ET DE FROTTEMENT
!
        lambdc = zr(jmu+iliac-1)
        lambdf = coefff*lambdc
!
! ----- PLAFONNEMENT GLISSEMENT
!
        if (glis .le. (glimax*glitol)) glis = glimin
!
! ----- ACTIVATION GLISSEMENT/ADHERENCE
!
        zr(jmu+3*nbliai+iliai-1) = sqrt(lambdf/glis)
        if (sqrt(lambdf/glis) .ge. sqrt(coefpt)) then
            zr(jmu+3*nbliai+iliai-1) = sqrt(coefpt)
        endif
100  end do
!
    call jedema()
!
end subroutine

subroutine cfafmu(resoco, neq, nbliai, nbliac, llf)
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
    include 'jeveux.h'
    include 'asterfort/calatm.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: neq
    integer :: nbliac, nbliai, llf
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! CALCUL DE AFMU - VECTEUR DES FORCES DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
!              (SERT A DECALER LES POINTEURS POUR LE FROTTEMENT 3D)
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!                'E': RESOCO(1:14)//'.ATMU'
! IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
! IN  FROT   : VAUT 1 LORSQU'IL Y A DU FROTTEMENT, 0 SINON
!
!
!
!
    integer :: compt0, jdecal, nbddl
    integer :: iliac, iliai
    real(kind=8) :: lambdc, zmu
    character(len=19) :: liac, mu, afmu, typl
    integer :: jliac, jmu, jafmu, jtypl
    character(len=24) :: appoin, apddl
    character(len=24) :: apcofr
    integer :: japptr, japddl
    integer :: japcof
    character(len=2) :: typlia, typec0
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    compt0 = 0
    typec0 = 'C0'
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    mu = resoco(1:14)//'.MU'
    afmu = resoco(1:14)//'.AFMU'
    typl = resoco(1:14)//'.TYPL'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcofr, 'L', japcof)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(mu, 'L', jmu)
    call jeveuo(afmu, 'E', jafmu)
!
! --- CALCUL
!
    do 140 iliac = 1, nbliac + llf
        typlia = zk8(jtypl-1+iliac)(1:2)
        if (typlia .eq. typec0) then
            compt0 = compt0 + 1
            iliai = zi(jliac+iliac-1)
            jdecal = zi(japptr+iliai-1)
            nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
            lambdc = zr(jmu+compt0-1)
            zmu = lambdc * zr(jmu+3*nbliai+iliai-1)
            call calatm(neq, nbddl, zmu, zr(japcof+jdecal), zi(japddl+ jdecal),&
                        zr(jafmu))
        endif
140  end do
!
    call jedema()
end subroutine

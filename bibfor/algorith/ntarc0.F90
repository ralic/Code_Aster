subroutine ntarc0(result, modele, mate, carele, sdcrit,&
                  lisch2, lnonl, para, numarc, instan)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rssepa.h'
    character(len=8) :: result
    integer :: numarc
    logical :: lnonl
    real(kind=8) :: instan, para(*)
    character(len=19) :: lisch2, sdcrit
    character(len=24) :: modele, mate, carele
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_* (ALGORITHME - ARCHIVAGE)
!
! ARCHIVAGE DES PARAMETRES
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
! IN  MODELE : NOM DU MODELE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  LNONL  : .TRUE. SI NON-LINEAIRE
! IN  PARA   : PARAMETRES DU CALCUL
!               (1) THETA
!               (2) DELTAT
! IN  SDCRIT : VALEUR DES CRITERES DE CONVERGENCE
! IN  LISCH2 : NOM DE LA SD INFO CHARGE POUR STOCKAGE DANS LA SD
! IN  NUMARC : NUMERO D'ARCHIVAGE
! IN  INSTAN : VALEUR DE L'INSTANT
!
!
!
!
    character(len=8) :: k8bid
    integer :: jpara, jcrr, jcrk, jinst
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ARCHIVAGE DE L'INSTANT
!
    call rsadpa(result, 'E', 1, 'INST', numarc,&
                0, jinst, k8bid)
    zr(jinst) = instan
!
! --- ARCHIVAGE DE DELTAT ET PARM_THETA
!
    call rsadpa(result, 'E', 1, 'PARM_THETA', numarc,&
                0, jpara, k8bid)
    zr(jpara) = para(1)
    call rsadpa(result, 'E', 1, 'DELTAT', numarc,&
                0, jpara, k8bid)
    zr(jpara) = para(2)
!
! --- ARCHIVAGE DES CRITERES DE CONVERGENCE
!
    if (lnonl) then
        call jeveuo(sdcrit(1:19)//'.CRTR', 'L', jcrr)
        call jeveuo(sdcrit(1:19)//'.CRDE', 'L', jcrk)
        call rsadpa(result, 'E', 1, zk16(jcrk), numarc,&
                    0, jpara, k8bid)
        zi(jpara) = nint(zr(jcrr))
        call rsadpa(result, 'E', 1, zk16(jcrk+1), numarc,&
                    0, jpara, k8bid)
        zi(jpara) = nint(zr(jcrr+1))
        call rsadpa(result, 'E', 1, zk16(jcrk+2), numarc,&
                    0, jpara, k8bid)
        zr(jpara) = zr(jcrr+2)
        call rsadpa(result, 'E', 1, zk16(jcrk+3), numarc,&
                    0, jpara, k8bid)
        zr(jpara) = zr(jcrr+3)
    endif
!
! --- ARCHIVAGE DU MODELE, MATERIAU, CARA_ELEM ET DE LA SD CHARGE
!
    call rssepa(result, numarc, modele(1:8), mate(1:8), carele(1:8),&
                lisch2)
!
    call jedema()
end subroutine

subroutine cclpco(option, resuou, numord, nbpaou, lipaou,&
                  lichou)
    implicit none
!     --- ARGUMENTS ---
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rsexch.h'
    integer :: nbpaou, numord
    character(len=8) :: resuou
    character(len=8) :: lipaou(*)
    character(len=16) :: option
    character(len=24) :: lichou(*)
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
! ----------------------------------------------------------------------
!  CALC_CHAMP - DETERMINATION LISTE DE PARAMETRES ET LISTE DE CHAMPS OUT
!  -    -                     -        -                      -      -
! ----------------------------------------------------------------------
!
! IN  :
!   OPTION  K16  NOM DE L'OPTION A CALCULER
!   RESUOU  K8   NOM DE LA STRUCUTRE DE DONNEES RESULTAT OUT
!   NUMORD  I    NUMERO D'ORDRE COURANT
!
! OUT :
!   NBPAOU  I    NOMBRE DE PARAMETRES OUT
!   LIPAOU  K8*  LISTE DES PARAMETRES OUT
!   LICHOU  K8*  LISTE DES CHAMPS OUT
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    integer :: opt, iaopds, iaoplo, iapara, nparin, ipara, ierd
    integer :: nparou
!
    character(len=19) :: nochou
!
    call jemarq()
!
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), opt)
    call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', iaopds)
    call jeveuo(jexnum('&CATA.OP.LOCALIS', opt), 'L', iaoplo)
    call jeveuo(jexnum('&CATA.OP.OPTPARA', opt), 'L', iapara)
!
    nparin = zi(iaopds-1+2)
    nparou = zi(iaopds-1+3)
!
    nbpaou = 0
!
    nparou = 1
!
!     BOUCLE SUR LES PARAMETRES DE L'OPTION
    do 10 ipara = 1, nparou
        nbpaou = nbpaou + 1
        lipaou(nbpaou) = zk8(iapara+nparin+ipara-1)
!
        call rsexch(' ', resuou, option, numord, nochou,&
                    ierd)
        lichou(nbpaou) = nochou
10  end do
!
    call jedema()
!
end subroutine

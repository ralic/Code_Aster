subroutine mdchin(nofimd, idfimd, nochmd, typent, typgeo,&
                  prefix, nbtv, codret)
!_____________________________________________________________________
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
! person_in_charge: nicolas.sellenet at edf.fr
! ======================================================================
!     FORMAT MED - CHAMP - INFORMATIONS - FICHIER CONNU PAR NOM
!            --    --      -                                -
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! !!! ATTENTION, CETTE ROUTINE NE DOIT PAS ETRE UTILISEE DANS LE  !!!
! !!! CAS D'UN ENRICHISSEMENT D'UN FICHIER MED CAR MDCHII SUPPOSE !!!
! !!! QUE LE FICHIER MED NE CHANGE PAS AU COURS DE LA COMMANDE    !!!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     DONNE LE NOMBRE DE TABLEAUX DE VALEURS ET LEURS CARACTERISTIQUES
!     TEMPORELLES POUR UN CHAMP ET UN SUPPORT GEOMETRIQUE
!-----------------------------------------------------------------------
!      ENTREES:
!        NOFIMD : NOM DU FICHIER MED
!        IDFIMD : OU NUMERO DU FCHIER MED DEJA OUVERT
!        NOCHMD : NOM MED DU CHAMP A LIRE
!        TYPENT : TYPE D'ENTITE AU SENS MED
!        TYPGEO : TYPE DE SUPPORT AU SENS MED
!      ENTREES/SORTIES:
!        PREFIX : BASE DU NOM DES STRUCTURES
!                 POUR LE TABLEAU NUMERO I
!                 PREFIX//'.NUME' : T(2I-1) = NUMERO DE PAS DE TEMPS
!                                   T(2I)   = NUMERO D'ORDRE
!                 PREFIX//'.INST' : T(I) = INSTANT S'IL EXISTE
!      SORTIES:
!        NBTV   : NOMBRE DE TABLEAUX DE VALEURS DU CHAMP
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_____________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#   include "asterfort/mdchii.h"
#   include "asterfort/as_mficlo.h"
#   include "asterfort/as_mfiope.h"
#   include "asterfort/u2mesg.h"
    integer :: nbtv
    integer :: typent, typgeo
    integer :: codret
!
    character(len=19) :: prefix
    character(len=*) :: nochmd
    character(len=*) :: nofimd
!
! 0.2. ==> VARIABLES LOCALES
!
    character(len=8) :: saux08
!
    integer :: edlect
    parameter (edlect=0)
!
    integer :: idfimd
    logical :: dejouv
!====
! 1. ON OUVRE LE FICHIER EN LECTURE
!====
!
    if (idfimd .eq. 0) then
        call as_mfiope(idfimd, nofimd, edlect, codret)
        dejouv = .false.
    else
        dejouv = .true.
        codret = 0
    endif
    if (codret .ne. 0) then
        saux08='mfiope'
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
!====
! 2. APPEL DU PROGRAMME GENERIQUE
!====
!
    call mdchii(idfimd, nochmd, typent, typgeo, prefix,&
                nbtv, codret)
!
!====
! 3. FERMETURE DU FICHIER MED
!====
!
    if (.not.dejouv) then
        call as_mficlo(idfimd, codret)
        if (codret .ne. 0) then
            saux08='mficlo'
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
        idfimd = 0
    endif
!
end subroutine

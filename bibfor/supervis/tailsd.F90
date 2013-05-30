subroutine tailsd(nom, nomsd, val, nbval)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/rsorac.h'
    integer :: nbval, val(nbval)
    character(len=*) :: nom, nomsd
! ---------------------------------------------------------------
!  DETERMINE LE NOMBRE DE VALEURS DANS UNE STRUCTURE DE DONNES
! ---------------------------------------------------------------
! IN  NOM     K*  NOM IDENTIFIANT A LA FOIS LA SD ET LA METHODE
! IN  NOMSD   I   NOM DE LA SD A INTERROGER
! OUT VAL     I   VECTEUR D'ENTIERS DE TAILLE NBVAL CONTENANT LES
!                 DIFFERENTES TAILLES
! IN  NBVAL   I   TAILLE DU VECTEUR D'ENTIERS VAL
! ---------------------------------------------------------------
    character(len=8) :: k8bid
    character(len=19) :: sd19
    character(len=24) :: sd
    integer :: iret1, iret2, ibid
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
!
!  DETERMINE LE NOMBRE MAXIMUM DE CHAMPS ET DE PARAMETRES
!  AINSI QUE LE NOMBRE EFFECTIF DE NUMEROS D'ORDRE POUR UNE SD RESULTAT
! ---------------------------------------------------------------------
    if (nom .eq. 'LIST_RESULTAT') then
        val(1) = 0
!       0 EST UTILISE POUR LES NUMEROS D'ORDRE (CF. RSACPA)
        val(2) = -1
        val(3) = 0
!
        sd19 = nomsd
        call jeexin(sd19 // '.DESC', iret1)
        call jeexin(sd19 // '.NOVA', iret2)
        if (iret1 .eq. 0 .or. iret2 .eq. 0) then
            goto 999
        endif
        call jelira(sd19 // '.DESC', 'NOMMAX', val(1), k8bid)
        call jelira(sd19 // '.NOVA', 'NOMMAX', val(2), k8bid)
        call rsorac(sd19, 'LONUTI', ibid, rbid, k8bid,&
                    cbid, rbid, ' ', val(3), 1,&
                    ibid)
!
!
!  DETERMINE LE NOMBRE D OBJETS SIMPLES DANS UNE COLECTION
! ---------------------------------------------------------------------
    else if (nom.eq.'LIST_COLLECTION') then
        val(1) = 0
!
        sd = nomsd
        call jeexin(sd, iret1)
        if (iret1 .eq. 0) then
            goto 999
        endif
        call jelira(sd, 'NUTIOC', val(1), k8bid)
        if (val(1) .eq. 0) call jelira(sd, 'NMAXOC', val(1), k8bid)
    endif
999  continue
end subroutine

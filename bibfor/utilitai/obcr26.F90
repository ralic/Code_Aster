subroutine obcr26(nomobj, nbparb, nbpari, nbparr, nbpark,&
                  nbparo)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include       'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/obcrcr.h'
    character(len=24) :: nomobj
    integer :: nbparb, nbpari, nbparr, nbpark, nbparo
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCTS)
!
! CREATION - SD IMPRESSION
!
! ----------------------------------------------------------------------
!
!
! IN  NOMOBJ : NOM DE L'OBJET
! OUT NBPARB : NOMBRE DE VALEURS DE TYPE BOOLEEN
! OUT NBPARI : NOMBRE DE VALEURS DE TYPE ENTIER
! OUT NBPARR : NOMBRE DE VALEURS DE TYPE REEL
! OUT NBPARK : NOMBRE DE VALEURS DE TYPE CHAINE
! OUT NBPARO : NOMBRE DE VALEURS DE TYPE OBJET
!
! ----------------------------------------------------------------------
!
    integer :: nparab, nparai, nparar, nparak, nparao
    parameter    (nparab=4,nparai=2,nparar=0,nparak=0,nparao=1)
    character(len=24) :: parab(nparab)
    character(len=24) :: parai(nparai)
    character(len=24) :: parar(nparar)
    character(len=24) :: parak(nparak)
    character(len=24) :: parao(nparao)
    character(len=24) :: typeo(nparao)
!
! ----------------------------------------------------------------------
!
!
! --- BOOLEEN
!
    data parab /'INFO_RESIDU'  ,'INFO_TEMPS'   ,'TABL_CONV_CSV',&
     &            'PRINT'        /
!
! --- ENTIER
!
    data parai /'UNIT_CONV_CSV','REAC_AFFICHAGE'/
!
! --- REEL
!
!      DATA PARAR //
!
! --- CHAINE
!
!      DATA PARAK //
!
! --- OBJETS
!
    data parao /'TABLEAU_CONV'/
!
! --- TYPE OBJETS
!
    data typeo /'TABLEAU'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbparb = nparab
    nbpari = nparai
    nbparr = nparar
    nbpark = nparak
    nbparo = nparao
!
! --- CREATION SD
!
    call obcrcr(nomobj, nbparb, nbpari, nbparr, nbpark,&
                nbparo, parab, parai, parar, parak,&
                parao, typeo)
!
    call jedema()
end subroutine

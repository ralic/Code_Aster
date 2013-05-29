subroutine nmaffm(sderro, sdimpr, nombcl)
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
    implicit     none
    include      'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmerge.h'
    include 'asterfort/nmimck.h'
    include 'asterfort/nmlecv.h'
    include 'asterfort/nmltev.h'
    include 'asterfort/obgeto.h'
    include 'asterfort/obtsdm.h'
    character(len=4) :: nombcl
    character(len=24) :: sdimpr, sderro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! AFFICHAGE DES MARQUES DANS LES COLONNES
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  SDIMPR : SD AFFICHAGE
! IN  NOMBCL : NOM DE LA BOUCLE
!               'RESI' - BOUCLE SUR LES RESIDUS D'EQUILIBRE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
!               'INST' - BOUCLE SUR LES PAS DE TEMPS
!               'CALC' - CALCUL
!
! ----------------------------------------------------------------------
!
    logical :: dvrela, dvmaxi, dvrefe, dvcomp
    logical :: dvpfix, dvfixc, dvfixf, dvfixg, dvfrot, dvcont, dvgeom
    logical :: dvdebo, cvpilo
    logical :: cvnewt, lerrne
    logical :: erctcg, erctcf, erctcc
    character(len=16) :: debors
    character(len=24) :: sdtabc, slcolo, lisnom
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- EVENEMENTS
!
    debors = ' DE BORST...    '
    call nmerge(sderro, 'DIVE_CTCC', dvcont)
    call nmerge(sderro, 'DIVE_DEBO', dvdebo)
    call nmerge(sderro, 'DIVE_RELA', dvrela)
    call nmerge(sderro, 'DIVE_MAXI', dvmaxi)
    call nmerge(sderro, 'DIVE_REFE', dvrefe)
    call nmerge(sderro, 'DIVE_COMP', dvcomp)
    call nmerge(sderro, 'DIVE_FROT', dvfrot)
    call nmerge(sderro, 'DIVE_GEOM', dvgeom)
    call nmerge(sderro, 'DIVE_PFIX', dvpfix)
    call nmerge(sderro, 'DIVE_FIXG', dvfixg)
    call nmerge(sderro, 'DIVE_FIXF', dvfixf)
    call nmerge(sderro, 'DIVE_FIXC', dvfixc)
    call nmerge(sderro, 'CONV_PILO', cvpilo)
    call nmlecv(sderro, 'NEWT', cvnewt)
    call nmltev(sderro, 'ERRI', 'NEWT', lerrne)
    call nmerge(sderro, 'ERRE_CTCG', erctcg)
    call nmerge(sderro, 'ERRE_CTCF', erctcf)
    call nmerge(sderro, 'ERRE_CTCC', erctcc)
!
! --- RECUPERATION DU TABLEAU DE CONVERGENCE
!
    call obgeto(sdimpr, 'TABLEAU_CONV', sdtabc)
!
! --- ACCES REPERTOIRE DE NOMS
!
    call obgeto(sdtabc, 'COLONNES_DISPOS', slcolo)
    call obgeto(slcolo, 'NOM_STRUCTS', lisnom)
!
! --- MISE A JOUR DES MARQUES DANS LES COLONNES
!
    if (nombcl .eq. 'NEWT') then
        call obtsdm(lisnom, 'RESI_RELA', ' ')
        call obtsdm(lisnom, 'RESI_MAXI', ' ')
        call obtsdm(lisnom, 'RESI_REFE', ' ')
        call obtsdm(lisnom, 'RESI_COMP', ' ')
        if (dvrela) call obtsdm(lisnom, 'RESI_RELA', 'X')
        if (dvmaxi) call obtsdm(lisnom, 'RESI_MAXI', 'X')
        if (dvrefe) call obtsdm(lisnom, 'RESI_REFE', 'X')
        if (dvcomp) call obtsdm(lisnom, 'RESI_COMP', 'X')
        call obtsdm(lisnom, 'GEOM_NEWT', ' ')
        call obtsdm(lisnom, 'FROT_NEWT', ' ')
        call obtsdm(lisnom, 'CONT_NEWT', ' ')
        call obtsdm(lisnom, 'PILO_COEF', ' ')
        call obtsdm(lisnom, 'CTCD_NBIT', ' ')
        if (dvgeom) call obtsdm(lisnom, 'GEOM_NEWT', 'X')
        if (dvfrot) call obtsdm(lisnom, 'FROT_NEWT', 'X')
        if (dvcont) call obtsdm(lisnom, 'CONT_NEWT', 'X')
        if (cvpilo) call obtsdm(lisnom, 'PILO_COEF', 'B')
        if (dvpfix) call obtsdm(lisnom, 'CTCD_NBIT', 'X')
        call obtsdm(lisnom, 'ITER_NUME', 'X')
        if (cvnewt) call obtsdm(lisnom, 'ITER_NUME', ' ')
        if (lerrne) call obtsdm(lisnom, 'ITER_NUME', 'E')
        call obtsdm(lisnom, 'BOUC_GEOM', 'X')
        call obtsdm(lisnom, 'BOUC_FROT', 'X')
        call obtsdm(lisnom, 'BOUC_CONT', 'X')
        if (dvdebo) call nmimck(sdimpr, 'DEBORST  ', debors, .true.)
    else if (nombcl.eq.'FIXE') then
        call obtsdm(lisnom, 'BOUC_GEOM', 'X')
        call obtsdm(lisnom, 'BOUC_FROT', 'X')
        call obtsdm(lisnom, 'BOUC_CONT', 'X')
        if (.not.dvfixg) then
            call obtsdm(lisnom, 'BOUC_GEOM', ' ')
        endif
        if (.not.dvfixf) then
            call obtsdm(lisnom, 'BOUC_FROT', ' ')
        endif
        if (.not.dvfixc) then
            call obtsdm(lisnom, 'BOUC_CONT', ' ')
        endif
        if (dvfixc) then
            call obtsdm(lisnom, 'BOUC_GEOM', 'X')
            call obtsdm(lisnom, 'BOUC_FROT', 'X')
        endif
        if (dvfixf) then
            call obtsdm(lisnom, 'BOUC_GEOM', 'X')
        endif
        if (erctcg) call obtsdm(lisnom, 'BOUC_GEOM', 'E')
        if (erctcf) call obtsdm(lisnom, 'BOUC_FROT', 'E')
        if (erctcc) call obtsdm(lisnom, 'BOUC_CONT', 'E')
    endif
!
    call jedema()
end subroutine

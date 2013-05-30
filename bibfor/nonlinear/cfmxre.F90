subroutine cfmxre(noma, nomo, sdstat, defico, resoco,&
                  numins, sddisc, solalg, valinc, veasse)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmmve.h'
    include 'asterfort/cfresu.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/diinst.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mmmcpt.h'
    include 'asterfort/mmmres.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/xmmres.h'
    character(len=24) :: resoco, defico, sdstat
    character(len=8) :: noma, nomo
    character(len=19) :: sddisc
    character(len=19) :: solalg(*), veasse(*), valinc(*)
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (POST_TRAITEMENT)
!
! REMPLIR LE VALE_CONT POUR L'ARCHIVAGE DU CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  SDSTAT : SD STATISTIQUES
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NOMO   : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMINS : NUMERO DU PAS DE CHARGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
!
!
!
    integer :: ifm, niv
    logical :: lctcc, lctcd, lxfcm, lexiv, lallv
    character(len=19) :: ddepla, depdel, depplu
    real(kind=8) :: inst(2)
    character(len=19) :: prno
    character(len=24) :: nochco
    integer :: jnochc
    character(len=19) :: cnsinr, cnsper, cnoinr
    integer :: ibid, iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
    lexiv = cfdisl(defico,'EXIS_VERIF')
    lallv = cfdisl(defico,'ALL_VERIF')
!
! --- NOM DES CHAM_NO
!
    nochco = resoco(1:14)//'.NOCHCO'
    call jeveuo(nochco, 'L', jnochc)
    cnsinr = zk24(jnochc+1-1)(1:19)
    cnoinr = zk24(jnochc+2-1)(1:19)
    cnsper = zk24(jnochc+3-1)(1:19)
!
! --- TOUT VERIF -> ON SAUTE
!
    if (lallv) then
        goto 50
    endif
!
! --- INSTANT
!
    inst(1) = diinst(sddisc,numins)
    inst(2) = inst(1) - diinst(sddisc,numins-1)
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
!
! --- POST-TRAITEMENT
!
    if (lxfcm) then
        call xmmres(depdel, nomo, veasse, cnsinr)
    else if (lctcc) then
        call mmmres(noma, inst, defico, resoco, depplu,&
                    depdel, sddisc, veasse, cnsinr, cnsper)
    else if (lctcd) then
        call cfresu(noma, numins, inst, sddisc, defico,&
                    resoco, depplu, depdel, ddepla, cnsinr,&
                    cnsper)
    else
        call assert(.false.)
    endif
!
! --- DECOMPTE NOMBRE DE LIAISONS
!
    if (lctcc) then
        call mmmcpt(noma, sdstat, defico, resoco, cnsinr)
    endif
!
! --- METHODE VERIF
!
50  continue
!
    if (lexiv) then
        call cfmmve(noma, defico, resoco, valinc)
    endif
!
! --- TRANSFO DU CHAM_NO_S EN CHAM_NO (AVEC UN PROF_CHNO CONSTANT !)
!
    call dismoi('C', 'PROF_CHNO', cnoinr, 'CHAM_NO', ibid,&
                prno, iret)
    call cnscno(cnsinr, prno, 'NON', 'V', cnoinr,&
                'F', ibid)
!
    call jedema()
end subroutine

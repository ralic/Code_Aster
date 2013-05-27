subroutine mdrevi(numddl, nbrevi, nbmode, bmodal, neq,&
                  dplrev, fonrev, ier)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/mgutdm.h'
    include 'asterfort/posddl.h'
    include 'asterfort/resmod.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbrevi, nbmode, neq, ier
    real(kind=8) :: dplrev(nbrevi, nbmode, *), bmodal(neq, *)
    character(len=8) :: fonrev(nbrevi, *)
    character(len=14) :: numddl
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     STOCKAGE DES INFORMATIONS DE REV DANS DES TABLEAUX
!     ------------------------------------------------------------------
! IN  : NUMDDL : NOM DU CONCEPT NUMDDL
! IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
! IN  : NBMODE : NOMBRE DE MODES DE LA BASE DE PROJECTION
! IN  : BMODAL : VECTEURS MODAUX
! IN  : NEQ    : NOMBRE D'EQUATIONS
! OUT : DPLREV : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE REV
! OUT : FONREV : TABLEAU DES FONCTIONS AUX NOEUDS DE REV
! OUT : IER    : CODE RETOUR
! ----------------------------------------------------------------------
!
!
!
!
!
    integer :: i, nunoe, nuddl, icomp
    character(len=8) :: noeu, comp, fonc, sst, noecho(3)
    character(len=14) :: nume
    character(len=16) :: typnum
    character(len=24) :: mdgene, mdssno, numero
    character(len=24) :: valk
    integer :: iarg
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, iret, j, jdpl, llrefe, nc, nf
    integer :: nn, ns
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
    call gettco(numddl, typnum)
!
    if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
        call jeveuo(numddl//'.NUME.REFE', 'L', llrefe)
        mdgene = zk24(llrefe)
        mdssno = mdgene(1:14)//'.MODG.SSNO'
        numero(1:14) = numddl
    endif
!
    do 10 i = 1, nbrevi
!
        call getvtx('RELA_EFFO_VITE', 'NOEUD', i, iarg, 1,&
                    noeu, nn)
        call getvtx('RELA_EFFO_VITE', 'NOM_CMP', i, iarg, 1,&
                    comp, nc)
        call getvid('RELA_EFFO_VITE', 'RELATION', i, iarg, 1,&
                    fonc, nf)
        call getvtx('RELA_EFFO_DEPL', 'SOUS_STRUC', i, iarg, 1,&
                    sst, ns)
!
        if (comp(1:2) .eq. 'DX') icomp = 1
        if (comp(1:2) .eq. 'DY') icomp = 2
        if (comp(1:2) .eq. 'DZ') icomp = 3
        if (comp(1:3) .eq. 'DRX') icomp = 4
        if (comp(1:3) .eq. 'DRY') icomp = 5
        if (comp(1:3) .eq. 'DRZ') icomp = 6
!
! ----- CALCUL DIRECT
        if (typnum .eq. 'NUME_DDL_SDASTER') then
            call posddl('NUME_DDL', numddl, noeu, comp, nunoe,&
                        nuddl)
!
! ----- CALCUL PAR SOUS-STRUCTURATION
        else if (typnum(1:13).eq.'NUME_DDL_GENE') then
            if (ns .eq. 0) then
                call u2mess('F', 'ALGORITH5_63')
            endif
            call jenonu(jexnom(mdssno, sst), iret)
            if (iret .eq. 0) then
                call u2mess('F', 'ALGORITH5_64')
            endif
            call mgutdm(mdgene, sst, ibid, 'NOM_NUME_DDL', ibid,&
                        nume)
            call posddl('NUME_DDL', nume(1:8), noeu, comp, nunoe,&
                        nuddl)
        endif
!
        if (nuddl .eq. 0) then
            valk = noeu
            call u2mesg('E+', 'ALGORITH15_16', 1, valk, 0,&
                        0, 0, 0.d0)
            if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
                valk = sst
                call u2mesg('E+', 'ALGORITH15_17', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
            valk = comp
            call u2mesg('E', 'ALGORITH15_18', 1, valk, 0,&
                        0, 0, 0.d0)
            ier = ier + 1
            goto 10
        endif
!
        do 11 j = 1, nbmode
            dplrev(i,j,1) = 0.d0
            dplrev(i,j,2) = 0.d0
            dplrev(i,j,3) = 0.d0
            dplrev(i,j,4) = 0.d0
            dplrev(i,j,5) = 0.d0
            dplrev(i,j,6) = 0.d0
11      continue
!
! ----- CALCUL DIRECT
        if (typnum .eq. 'NUME_DDL_SDASTER') then
            do 13 j = 1, nbmode
                dplrev(i,j,icomp) = bmodal(nuddl,j)
13          continue
!
! ----- CALCUL PAR SOUS-STRUCTURATION
        else if (typnum(1:13).eq.'NUME_DDL_GENE') then
            call wkvect('&&MDREVI.DPLCHO', 'V V R8', nbmode*6, jdpl)
            noecho(1) = noeu
            noecho(2) = sst
            noecho(3) = nume
            call resmod(bmodal, nbmode, neq, numero, mdgene,&
                        noecho, zr( jdpl))
            do 12 j = 1, nbmode
                dplrev(i,j,icomp) = zr(jdpl-1+j+(icomp-1)*nbmode)
12          continue
            call jedetr('&&MDREVI.DPLCHO')
        endif
!
        fonrev(i,1) = noeu
        fonrev(i,2) = comp
        fonrev(i,3) = fonc
!
10  end do
!
    call jedema()
end subroutine

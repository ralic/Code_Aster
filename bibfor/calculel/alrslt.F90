subroutine alrslt(iopt, ligrel, nout, lchout, lpaout,&
                  base, ldist, lfeti)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/alchml.h'
    include 'asterfort/alresl.h'
    include 'asterfort/assert.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisd.h'
    include 'asterfort/grdeur.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: iopt, nout
    character(len=19) :: ligrel
    character(len=*) :: base, lchout(*)
    character(len=8) :: lpaout(*)
    logical :: ldist, lfeti
! ----------------------------------------------------------------------
!     ENTREES:
!      IOPT : OPTION
!     LIGREL : NOM DE LIGREL
!     LCHOUT : LISTE DES NOMS DES CHAMPS DE SORTIE
!     LPAOUT : LISTE DES PARAMETRES ASSOCIES AUX CHAMPS DE SORTIE
!       NOUT : NOMBRE DE CHAMPS DE SORTIE
!       BASE : 'G', 'V' OU 'L'
!     LDIST : CALCUL DISTRIBUE
!     LFETI : CALCUL FETI
!     SORTIES:
!      CREATION DES CHAMPS GLOBAUX RESULTATS
!
! ----------------------------------------------------------------------
    common /caii05/ianoop,ianote,nbobtr,iaobtr,nbobmx
    common /caii07/iachoi,iachok
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
!     FONCTIONS EXTERNES:
!     -------------------
    integer :: ianoop, ianote, nbobtr, iaobtr, nbobmx, iachoi, iachok
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: gd, descgd, code, i, iret1, iret2, ibid, iret, jcelk, jnoli
    character(len=19) :: nochou, dcel
    character(len=8) :: nompar
    character(len=8) :: kbi1, kbi2
    character(len=16) :: nomopt
!
!
    call jenuno(jexnum('&CATA.OP.NOMOPT', iopt), nomopt)
!
!
!     -- ALLOCATION DES CHAMPS RESULTATS :
    do 10 i = 1, nout
        nompar = lpaout(i)
        nochou = lchout(i)
        gd = grdeur(nompar)
        call jeveuo(jexnum('&CATA.GD.DESCRIGD', gd), 'L', descgd)
        code = zi(descgd-1+1)
!
        call detrsd('CHAMP', nochou)
!        -- SI GD EST 1 GRANDEUR_SIMPLE --> CHAM_ELEM
        if (code .eq. 1) then
            call exisd('CHAM_ELEM_S', nochou, iret)
            if (iret .gt. 0) then
                dcel = nochou
            else
                dcel = ' '
            endif
            call alchml(ligrel, nomopt, nompar, base, nochou,&
                        iret, dcel)
!        -- LES CHAM_ELEMS SONT INCOMPLETS AVEC FETI OU LDIST
            if (ldist .or. lfeti) then
                call jeveuo(nochou//'.CELK', 'E', jcelk)
                zk24(jcelk-1+7)='MPI_INCOMPLET'
            endif
!
        else
!        -- SINON --> RESUELEM
            call assert((code.ge.3).and.(code.le.5))
            call alresl(iopt, ligrel, nochou, nompar, base)
!        -- LES RESU_ELEMS SONT COMPLETS AVEC FETI (MATRICE/ SM LOCAUX)
!           MAIS INCOMPLETS EN LDIST (BLOCS DE MATRICE/SM GLOBAUX)
            if (ldist) then
                call jeveuo(nochou//'.NOLI', 'E', jnoli)
                zk24(jnoli-1+3)='MPI_INCOMPLET'
            endif
        endif
10  end do
!
!
!     --MISE A JOUR DE CAII07 :
    call wkvect('&&CALCUL.LCHOU_I', 'V V I', max(3*nout, 3), iachoi)
    nbobtr = nbobtr + 1
    zk24(iaobtr-1+nbobtr) = '&&CALCUL.LCHOU_I'
    call wkvect('&&CALCUL.LCHOU_K8', 'V V K8', max(2*nout, 2), iachok)
    nbobtr = nbobtr + 1
    zk24(iaobtr-1+nbobtr) = '&&CALCUL.LCHOU_K8'
!
    do 30 i = 1, nout
        nompar = lpaout(i)
        nochou = lchout(i)
        gd = grdeur(nompar)
        call jeveuo(jexnum('&CATA.GD.DESCRIGD', gd), 'L', descgd)
        code = zi(descgd-1+1)
        call jeexin(nochou//'.DESC', iret1)
        call jeexin(nochou//'.CELD', iret2)
        if ((iret1+iret2) .eq. 0) goto 30
!
        call dismoi('F', 'NOM_GD', nochou, 'CHAMP', ibid,&
                    kbi1, ibid)
        call dismoi('F', 'TYPE_SCA', kbi1, 'GRANDEUR', ibid,&
                    kbi2, ibid)
        zk8(iachok-1+2* (i-1)+2) = kbi2
        call dismoi('F', 'TYPE_CHAMP', nochou, 'CHAMP', ibid,&
                    kbi1, ibid)
!        -- SI C'EST UN CHAM_ELEM:
!           ON DOIT FAIRE UN JEVEUO EN ECRITURE POUR RECUPERER
!           L'ADRESSE DU .CELV
        if (kbi1(1:2) .eq. 'EL') then
            call jeveuo(nochou//'.CELD', 'E', zi(iachoi-1+3* (i-1)+1))
            call jeveuo(nochou//'.CELV', 'E', zi(iachoi-1+3* (i-1)+2))
            zk8(iachok-1+2* (i-1)+1) = 'CHML'
        else
            call jeveuo(nochou//'.DESC', 'E', zi(iachoi-1+3* (i-1)+1))
            if (evfini .eq. 1 .and. code .gt. 3) then
                call assert(code.eq.5)
                call jeveuo(nochou//'.RSVI', 'L', zi(iachoi-1+3* (i-1)+ 2))
                call jeveuo(jexatr(nochou//'.RSVI', 'LONCUM'), 'L', zi(iachoi-1+3* (i-1)+3))
            endif
            zk8(iachok-1+2* (i-1)+1) = 'RESL'
        endif
30  end do
!
end subroutine

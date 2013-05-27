subroutine cafond(char, ligrmo, ialloc, noma, fonree)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterfort/alcart.h'
    include 'asterfort/calcul.h'
    include 'asterfort/codent.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/exlim1.h'
    include 'asterfort/foc1su.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mesomm.h'
    include 'asterfort/nocart.h'
    include 'asterfort/peair1.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    integer :: ialloc
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
!-----------------------------------------------------------------------
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
! BUT : STOCKAGE DE EFFE_FOND DANS LA CARTE PRES ALLOUEE SUR LE
!       LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      IALLOC : 1 SI LA CARTE DE PRESSION ALLOUE PAR CAPRES, 0 SINON
!      NOMA   : NOM DU MAILLAGE
!      FONREE : FONC OU REEL
!
!-----------------------------------------------------------------------
    integer :: npres, jncmp, jvalv, ncmp, iocc, npr, iatyma, i, j, ima, iadtyp
    integer :: jmai, jmap, ibid, ifm, niv, lval1, nbpt, nbmai, nbmap
    real(kind=8) :: aire, long, eff, pint, smat, iner(10), cmult
    complex(kind=8) :: cbid
    character(len=1) :: k1bid
    character(len=3) :: kocc
    character(len=8) :: k8b, maille, type, lpain(2), lpaout(2), typmcl(3)
    character(len=8) :: fpint, foeff
    character(len=16) :: motclf, motcle(3)
    character(len=19) :: carte, ligrel
    character(len=24) :: lchin(2), lchout(2), mesmai, mesmap, vale
    character(len=24) :: valk(4)
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
    motclf = 'EFFE_FOND'
    call getfac(motclf, npres)
!
    carte = char//'.CHME.PRESS'
    if (ialloc .eq. 0) then
        if (fonree .eq. 'REEL') then
            call alcart('G', carte, noma, 'PRES_R')
        else if (fonree.eq.'FONC') then
            call alcart('G', carte, noma, 'PRES_F')
        else
            call u2mesk('F', 'MODELISA2_37', 1, fonree)
        endif
    endif
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE PRESSIONS NULLES SUR TOUT LE MAILLAGE
!
    ncmp = 1
    zk8(jncmp) = 'PRES'
    if (ialloc .eq. 0) then
        if (fonree .eq. 'REEL') then
            zr(jvalv) = 0.d0
        else
            zk8(jvalv) = '&FOZERO'
        endif
        call nocart(carte, 1, ' ', 'NOM', 0,&
                    ' ', 0, ligrmo, ncmp)
    endif
!
    mesmai = '&&CAFOND.MAILLES_INTE'
    mesmap = '&&CAFOND.MAILLES_PRES'
    motcle(1) = 'GROUP_MA_INT'
    motcle(2) = 'GROUP_MA'
    motcle(3) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'GROUP_MA'
    typmcl(3) = 'MAILLE'
!
    ligrel = '&&CAFOND.LIGREL'
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpaout(1) = 'PCASECT'
    lchout(1) = '&&CAFOND.PSECT'
!
    do 20 iocc = 1, npres
!
! ------ ENSEMBLE DES MAILLES MODELISANT LE CONTOUR DE TUYAUTERIE
!        POUR CALCULER L'AIRE DU TROU
!
        call reliem(' ', noma, 'NU_MAILLE', motclf, iocc,&
                    1, motcle(1), typmcl(1), mesmai, nbmai)
        call jeveuo(mesmai, 'L', jmai)
!
! ------ ENSEMBLE DES MAILLES MODELISANT LA SECTION DE TUYAUTERIE
!        POUR APPLIQUER LA PRESSION
!
        call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                    2, motcle(2), typmcl(2), mesmap, nbmap)
        call jeveuo(mesmap, 'L', jmap)
!
        call exlim1(zi(jmap), nbmap, ligrmo, 'V', ligrel)
!
! ------ CALCUL DE L'EFFET DE FOND POUR CHAQUE OCCURENCE
!
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'PRES', iocc, iarg, 1,&
                        pint, npr)
        else
            call getvid(motclf, 'PRES', iocc, iarg, 1,&
                        fpint, npr)
        endif
!
! ------ BORD DU TROU : CALCUL DE L'AIRE
!
        call peair1(ligrmo, nbmai, zi(jmai), aire, long)
!
! ------ CALCUL SUR CHAQUE ELEMENT DES CARACTERISTIQUES GEOMETRIQUES :
!        SOMME/S_ELEMENT(1,X,Y,Z,X*X,Y*Y,Z*Z,X*Y,X*Z,Y*Z)DS
!
        call calcul('S', 'CARA_SECT_POUT3', ligrel, 1, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
! ------ VECTEUR DES QUANTITES GEOMETRIQUES PRECITEES SOMMEES
!        SUR LA SURFACE, CES QUANTITES SERONT NOTEES :
!        A1 = S,AX,AY,AZ,AXX,AYY,AZZ,AXY,AXZ,AYZ
!
! ------ SOMMATION DES QUANTITES GEOMETRIQUES ELEMENTAIRES
!
        call mesomm(lchout(1), 10, ibid, iner, cbid,&
                    0, ibid)
!
        smat = iner(1)
        if (fonree .eq. 'REEL') then
            eff = -pint/smat*aire
            zr(jvalv) = eff
            if (niv .eq. 2) then
                write (ifm,*) 'SURFACE DU TROU    ',aire
                write (ifm,*) 'SURFACE DE MATIERE ',smat
                write (ifm,*) 'EFFET DE FOND ',eff
            endif
        else
            call codent(iocc, 'G', kocc)
            foeff = '&FOEF'//kocc
            cmult = -1.d0/smat*aire
            call foc1su(foeff, 1, fpint, cmult, cbid,&
                        'FONCTION', .false., .false., ' ', 'G')
            zk8(jvalv) = foeff
            if (niv .eq. 2) then
                write (ifm,*) 'SURFACE DU TROU    ',aire
                write (ifm,*) 'SURFACE DE MATIERE ',smat
                write (ifm,*) 'EFFET DE FOND ',foeff,':'
                vale(1:19) = foeff
                vale(20:24) = '.VALE'
                call jelira(vale, 'LONUTI', nbpt, k1bid)
                call jeveuo(vale, 'L', lval1)
                nbpt = nbpt/2
                do 5 i = 1, nbpt
                    write(ifm,*) '  ',zr(lval1+i-1),',',zr(lval1+nbpt+i-1)
 5              continue
            endif
        endif
!
        call jedetr('&&CAFOND.LISTE_MAILLES')
        call detrsd('LIGREL', ligrel)
!
! ------ AFFECTATION DE LA PRESSION CORRESPONDANTE AUX MAILLES
!
        do 10 j = 1, nbmap
            ima = zi(jmap-1+j)
            iadtyp = iatyma - 1 + ima
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
            if ((type(1:4).ne.'QUAD') .and. (type(1:4).ne.'TRIA')) then
                call jenuno(jexnum(noma//'.NOMMAI', ima), maille)
                valk (1) = maille
                valk (2) = 'QUAD'
                valk (3) = 'TRIA'
                valk (4) = motclf
                call u2mesg('A', 'MODELISA8_40', 4, valk, 0,&
                            0, 0, 0.d0)
            endif
10      continue
        call nocart(carte, 3, k8b, 'NUM', nbmap,&
                    k8b, zi(jmap), ' ', ncmp)
!
        call jedetr(mesmai)
        call jedetr(mesmap)
!
20  end do
!
    call jedetr(char//'.PRES.GROUP')
    call jedetr(char//'.PRES.LISTE')
!-----------------------------------------------------------------------
    call jedema()
end subroutine

subroutine aflrch(lisrez, chargz)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
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
    include 'jeveux.h'
!
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/cragch.h'
    include 'asterfort/craglc.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/elimdi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/impre2.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/nocart.h'
    include 'asterfort/noligr.h'
    include 'asterfort/ordlrl.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=19) :: lisrel
    character(len=8) :: charge
    character(len=*) :: lisrez, chargz
! -------------------------------------------------------
!     AFFECTATION DE L'OBJET DE TYPE  LISTE_RELA ET DE NOM
!     LISREL A L'OBJET DE TYPE CHARGE ET DE NOM CHARGE
! -------------------------------------------------------
!     SI LA SD_LISTE_RELA N'EXISTE PAS, ON NE FAIT RIEN.
!     LA CHARGE DOIT EXISTER AU PREALABLE
!
!     la sd_liste_rela EST DETRUITe A LA FIN DE LA ROUTINE
! -------------------------------------------------------
!  LISREL        - IN    - K24  - : NOM DE LA SD LISTE_RELA
!                - JXVAR -      -
! -------------------------------------------------------
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -
! -------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
    integer :: nmocl
    parameter(nmocl=300)
    complex(kind=8) :: betac
    character(len=4) :: typval, typcoe
    character(len=24) :: valk(2)
    character(len=7) :: typcha
    character(len=19) :: betaf
    character(len=8) :: mod, nomgd, nomnoe, kbid
    character(len=8) :: noma, cmp, nomcmp(nmocl)
    character(len=9) :: nomte
    character(len=19) :: ca1, ca2
    character(len=19) :: ligrmo, ligrch
    integer :: ntypel(nmocl)
    integer :: vali(2)
    character(len=1) :: k1bid
    real(kind=8) :: beta
    integer :: i, ibid, icmp, iddl, idecal, ier, ierd, ifm, igrel
    integer :: in, indsur, inema, inema0, ino, inom, ipntrl, irela
    integer :: iret, j, jnbno, jncmp1, jncmp2, jnoma, jprnm, jrlbe, jrlco
    integer :: jrlcof, jrldd, jrlla, jrlno, idnoeu, jrlnr, jrlnt, jrlpo
    integer :: jrlsu, jrltc, jrltv, jvale1, jvale2, jvalv1, jvalv2, kddl
    integer :: nbcmp, nbec, nbnema, nbrela, nbteli, nbterm, nddla
    integer :: niv, numel, nunewm, nbdual, nbsurc, iexi
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
    call jemarq()
    lisrel=lisrez
    call jeexin(lisrel//'.RLCO', iexi)
    if (iexi .eq. 0) goto 9999
!
    charge=chargz
    call infniv(ifm, niv)
!
!
    call dismoi('F', 'NOM_MODELE', charge, 'CHARGE', ibid,&
                mod, ier)
    ligrmo=mod(1:8)//'.MODELE'
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma=zk8(jnoma)
    call dismoi('F', 'TYPE_CHARGE', charge, 'CHARGE', ibid,&
                typcha, ier)
!
    if (typcha(1:4) .eq. 'MECA') then
        ligrch=charge//'.CHME.LIGRE'
        nomgd='DEPL_R'
        nomte='D_DEPL_R_'
    else if (typcha(1:4).eq.'THER') then
        ligrch=charge//'.CHTH.LIGRE'
        nomgd='TEMP_R'
        nomte='D_TEMP_R_'
    else if (typcha(1:4).eq.'ACOU') then
        ligrch=charge//'.CHAC.LIGRE'
        nomgd='PRES_C'
        nomte='D_PRES_C_'
    endif
!
!
!     -- REARRANGEMENT DES RELATIONS DE LA LISTE DE RELATIONS
!     -- PAR ORDRE DE NUMERO DE NOEUD CROISSANT
!     -- ET SUPPRESSION DES RELATIONS REDONDANTES EN
!     -- APPLIQUANT LE PRINCIPE DE SURCHARGE
    call ordlrl(charge, lisrel, nomgd)
!
!     -- TRAITEMENT DU MOT CLE METHODE='ELIMINATION':
    call elimdi(charge, lisrel, nomgd, nbdual, nbsurc)
    if (nbdual .eq. 0) goto 9998
!
!
    if (ligrch(12:13) .eq. 'TH') then
        ca1=charge//'.CHTH.CMULT'
        ca2=charge//'.CHTH.CIMPO'
    else if (ligrch(12:13).eq.'ME') then
        ca1=charge//'.CHME.CMULT'
        ca2=charge//'.CHME.CIMPO'
    else if (ligrch(12:13).eq.'AC') then
        ca1=charge//'.CHAC.CMULT'
        ca2=charge//'.CHAC.CIMPO'
    endif
!
    call jeveuo(lisrel//'.RLTC', 'L', jrltc)
    typcoe=zk8(jrltc)(1:4)
!
! --- TYPE DES VALEURS AU SECOND MEMBRE DES RELATIONS
    call jeveuo(lisrel//'.RLTV', 'L', jrltv)
    typval=zk8(jrltv)(1:4)
!
! --- NOMBRE DE RELATIONS DE LA LISTE DE RELATIONS
    call jeveuo(lisrel//'.RLNR', 'L', jrlnr)
    nbrela=zi(jrlnr)
!
! --- NOMBRE TOTAL DE TERMES IMPLIQUES DANS LES RELATIONS
! --- DE LA LISTE DE RELATIONS (SERT AU REDIMENSIONNEMENT
! --- DU LIGREL DE CHARGE ET DES CARTES .CMULT ET .CIMPO
! --- DE LA CHARGE)
    call jeveuo(lisrel//'.RLPO', 'L', jrlpo)
    nbteli=zi(jrlpo+nbrela-1)
!
! --- VERIFICATION DE L'ADEQUATION DE LA TAILLE DU LIGREL DE
! --- CHARGE A SON AFFECTATION PAR LES MAILLES TARDIVES DUES
! --- AUX RELATIONS LINEAIRES
! --- SI LE LIGREL DE CHARGE N'EXISTE PAS, ON LE CREE
    call craglc(nbteli, ligrch)
!
! --- VERIFICATION DE L'ADEQUATION DE LA TAILLE DES CARTES
! --- .CMULT ET .CIMPO DE LA CHARGE A LEUR AFFECTATION
! --- PAR LES MAILLES TARDIVES DUES AUX RELATIONS LINEAIRES
! --- SI LES CARTES .CMULT ET .CIMPO N'EXISTENT PAS, ON
! --- LES CREE
    call cragch(nbteli, typcoe, typval, ligrch)
!
!
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nbcmp, k1bid)
    nddla=nbcmp-1
    if (nddla .gt. nmocl) then
        vali(1)=nmocl
        vali(2)=nddla
        call u2mesg('F', 'MODELISA8_29', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
    call assert(nbcmp.le.nmocl)
    do 10 i = 1, nbcmp
        nomcmp(i)=zk8(inom-1+i)
        call jenonu(jexnom('&CATA.TE.NOMTE', nomte//nomcmp(i)(1:7)), ntypel(i))
10  end do
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nbec,&
                kbid, ierd)
!
!
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    else
        call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    endif
!
! --- LES CARTES CA1 ET CA2 DOIVENT OBLIGATOIREMENT AVOIR ETE
! --- CREEES AU PREALABLE
    call jeexin(ca1//'.DESC', iret)
    if (iret .eq. 0) call u2mesk('F', 'MODELISA_95', 1, ca1)
    call jeveuo(ca1//'.NCMP', 'E', jncmp1)
    call jeveuo(ca1//'.VALV', 'E', jvalv1)
    call jeveuo(ca1//'.VALE', 'E', jvale1)
    call jeveuo(ca2//'.NCMP', 'E', jncmp2)
    call jeveuo(ca2//'.VALV', 'E', jvalv2)
    call jeveuo(ca2//'.VALE', 'E', jvale2)
!
!
    numel=0
    call jeveuo(ligrch//'.NBNO', 'E', jnbno)
    call dismoi('F', 'NB_MA_SUP', ligrch, 'LIGREL', inema,&
                kbid, ier)
    call dismoi('F', 'NB_GREL', ligrch, 'LIGREL', igrel,&
                kbid, ier)
!
!
    call jeveuo(lisrel//'.RLCO', 'L', jrlco)
    call jeveuo(lisrel//'.RLDD', 'L', jrldd)
    call jeveuo(lisrel//'.RLNO', 'L', jrlno)
    call jeveuo(lisrel//'.RLNT', 'L', jrlnt)
    call jeveuo(lisrel//'.RLPO', 'L', jrlpo)
    call jeveuo(lisrel//'.RLSU', 'L', jrlsu)
    call jeveuo(lisrel//'.RLBE', 'L', jrlbe)
    call jeveuo(lisrel//'.RLLA', 'L', jrlla)
!
!
!
    do 60 irela = 1, nbrela
        indsur=zi(jrlsu+irela-1)
        if (indsur .ne. 0) goto 60
!
        ipntrl=zi(jrlpo+irela-1)
        nbterm=zi(jrlnt+irela-1)
        if (typval .eq. 'REEL') then
            beta=zr(jrlbe+irela-1)
        else if (typval.eq.'COMP') then
            betac=zc(jrlbe+irela-1)
        else if (typval.eq.'FONC') then
            betaf=zk24(jrlbe+irela-1)(1:19)
        endif
        idecal=ipntrl-nbterm
        jrlcof=jrlco+idecal
        idnoeu=jrlno+idecal
        iddl=jrldd+idecal
!
        numel=0
        inema0=inema
        zi(jnbno)=zi(jnbno)+2
        do 40 ino = 1, nbterm
            nomnoe=zk8(idnoeu+ino-1)
            call jenonu(jexnom(noma//'.NOMNOE', nomnoe), in)
!
            cmp=zk8(iddl+ino-1)
!
            icmp=indik8(nomcmp,cmp,1,nbcmp)
            if (.not.exisdg(zi(jprnm-1+(in-1)*nbec+1),icmp)) then
                valk(1)=cmp
                valk(2)=nomnoe
                call u2mesg('F', 'MODELISA8_30', 2, valk, 0,&
                            0, 0, 0.d0)
            else
                do 20 kddl = 1, nbcmp
                    if (cmp .eq. nomcmp(kddl)) then
                        numel=ntypel(kddl)
                        goto 30
!
                    endif
20              continue
            endif
30          continue
!
            if (numel .ne. 0) then
                igrel=igrel+1
                call noligr(ligrch, igrel, numel, 1, in,&
                            '        ', 3, 1, inema, zi(jnbno),&
                            zk8(jrlla+irela-1))
            else
                call u2mesk('F', 'AFFECHARMECA_1', 1, nomnoe)
            endif
40      continue
!
!
!
!       --  STOCKAGE DANS LES CARTES CA1 ET CA2
        nbnema=inema-inema0
        if (nbnema .ne. nbterm) then
            call u2mess('F', 'AFFECHARMECA_2')
        endif
        zk8(jncmp1)='A1'
        zk8(jncmp2)='C'
        do 50 j = 1, nbnema
            if (typcoe .eq. 'COMP') then
                zc(jvalv1)=zc(jrlcof-1+j)
            else
                zr(jvalv1)=zr(jrlcof-1+j)
            endif
            nunewm=-(inema0+j)
            call nocart(ca1, -3, ' ', 'NUM', 1,&
                        ' ', nunewm, ligrch, 1)
            if (j .lt. nbnema) then
                if (typval .eq. 'REEL') then
                    zr(jvalv2)=0.d0
                else if (typval.eq.'COMP') then
                    zc(jvalv2)=(0.0d0,0.0d0)
                else
                    zk24(jvalv2)='&FOZERO'
                endif
            else
                if (typval .eq. 'REEL') then
                    zr(jvalv2)=beta
                else if (typval.eq.'COMP') then
                    zc(jvalv2)=betac
                else
                    zk24(jvalv2)=betaf
                endif
            endif
            call nocart(ca2, -3, ' ', 'NUM', 1,&
                        ' ', nunewm, ligrch, 1)
50      continue
60  end do
!
!
!
!     -- IMPRESSION DES RELATIONS REDONDANTES ET DONC SUPPRIMEES :
!     ------------------------------------------------------------
9998  continue
    if ((nbsurc.gt.0) .and. (niv.ge.2)) then
        write (ifm,*)'LES RELATIONS SUIVANTES SONT '//&
     &    'REDONDANTES ET DONC SUPPRIMEES EN APPLIQUANT '//&
     &    'LE PRINCIPE DE SURCHARGE.'
!
        do 70 irela = 1, nbrela
            indsur=zi(jrlsu+irela-1)
            if (indsur .ne. 1) goto 70
            ipntrl=zi(jrlpo+irela-1)
            nbterm=zi(jrlnt+irela-1)
            if (typval .eq. 'REEL') then
                beta=zr(jrlbe+irela-1)
            else if (typval.eq.'COMP') then
                betac=zc(jrlbe+irela-1)
            else if (typval.eq.'FONC') then
                betaf=zk24(jrlbe+irela-1)(1:19)
            endif
            idecal=ipntrl-nbterm
            jrlcof=jrlco+idecal
            idnoeu=jrlno+idecal
            iddl=jrldd+idecal
            call impre2(lisrel//'.RLCO', lisrel//'.RLDD', lisrel// '.RLNO', lisrel//'.RLBE',&
                        zi(jrlsu+irela-1), zi(jrlpo+ irela-1), zi(jrlnt+irela-1), typcoe, typval,&
                        irela)
70      continue
    endif
!
    call jedetr(lisrel//'.RLCO')
    call jedetr(lisrel//'.RLDD')
    call jedetr(lisrel//'.RLNO')
    call jedetr(lisrel//'.RLBE')
    call jedetr(lisrel//'.RLNT')
    call jedetr(lisrel//'.RLPO')
    call jedetr(lisrel//'.RLSU')
    call jedetr(lisrel//'.RLNR')
    call jedetr(lisrel//'.RLTC')
    call jedetr(lisrel//'.RLTV')
    call jedetr(lisrel//'.RLLA')
!
9999  continue
    call jedema()
end subroutine

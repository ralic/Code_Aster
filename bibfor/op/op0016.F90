subroutine op0016()
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    implicit none
!
!     DIRECTIVE IMPR_JEVEUX
!
    include 'jeveux.h'
!
    include 'asterc/getvis.h'
    include 'asterc/getvtx.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeimpa.h'
    include 'asterfort/jeimpd.h'
    include 'asterfort/jeimpm.h'
    include 'asterfort/jeimpo.h'
    include 'asterfort/jeimpr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeprat.h'
    include 'asterfort/jepreg.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/uldefi.h'
    character(len=8) :: fich
    character(len=24) :: nomobj, nom
    character(len=32) :: noml32
    character(len=80) :: txt
    character(len=16) :: noment
    character(len=1) :: nomcla, k1bid
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: i, imes, info, ires, iret, iuni
    integer :: n, n1, n2, n3, nfic, nif, noc
    integer :: nrg, num, numerg, nuni
!-----------------------------------------------------------------------
    call infmaj()
    ires=iunifi('RESULTAT')
    imes=iunifi('MESSAGE')
    fich='MESSAGE'
    iuni=imes
!
    call getvis('IMPRESSION', 'UNITE', 1, iarg, 1,&
                iuni, nuni)
    call uldefi(iuni, ' ', ' ', 'A', 'N',&
                'O')
    call getvtx('IMPRESSION', 'NOM', 1, iarg, 1,&
                fich, nfic)
    if (nfic .ne. 0) then
!
        if (fich(1:8) .eq. 'RESULTAT') then
            iuni=ires
        else if (fich(1:7).eq.'MESSAGE') then
            iuni=imes
        else
            write(imes,*) fich,': FICHIER INCONNU'
            write(imes,*) 'LES ECRITURES SONT SUR LE FICHIER MESSAGE'
        endif
        if (nuni .ne. 0) then
            write(imes,*) 'CHOISIR ENTRE LES MOTS-CLE NOM ET UNITE'
            write(imes,*) 'LES ECRITURES SONT SUR LE FICHIER MESSAGE'
        endif
    endif
!
!
    call getvtx(' ', 'ENTITE', 0, iarg, 1,&
                noment, n)
    if (n .eq. 0) goto 9999
!
    call getvtx(' ', 'COMMENTAIRE', 0, iarg, 1,&
                txt, n)
    if (n .eq. 0) txt=' '
!
    if (noment(1:6) .eq. 'DISQUE') then
!
        nomcla = ' '
        call getvtx(' ', 'CLASSE', 0, iarg, 1,&
                    nomcla, n)
        write(iuni,*) ' '
        call jeimpd(imes, nomcla, txt)
        write(iuni,*) ' '
!
    else if (noment(1:7) .eq. 'MEMOIRE') then
!
        write(iuni,*) ' '
        call jeimpm(imes)
        write(iuni,*) ' '
!
    else if (noment .eq. 'REPERTOIRE') then
!
        nomcla = ' '
        call getvtx(' ', 'CLASSE', 0, iarg, 1,&
                    nomcla, n)
        write(iuni,*) ' '
        call jeimpr(imes, nomcla, txt)
        write(iuni,*) ' '
!
    else if (noment(1:5) .eq. 'OBJET') then
!
        call getvtx(' ', 'NOMOBJ', 0, iarg, 1,&
                    nomobj, n)
        noml32 = nomobj
        call jjvern(noml32, 0, iret)
        write(iuni,*) ' '
        write(iuni,*) ' '
        if (iret .eq. 0) then
            write(iuni,*) ' DIRECTIVE IMPR_JEVEUX '
            write(iuni,*) ' L''OBJET "',nomobj,'" N''EXISTE PAS'
            goto 9999
        else
            write(iuni,*) ' '
            write(iuni,*)' ECRITURE DE L''OBJET : "',nomobj,'"'
        endif
        call getvtx(' ', 'COMMENTAIRE', 0, iarg, 1,&
                    txt, n)
        if (n .eq. 0) txt=' '
        write(iuni,*) ' '
        call jeimpa(iuni, nomobj, txt)
        write(iuni,*) ' '
        write(iuni,*) ' '
        if (iret .eq. 2) then
            call getvis(' ', 'NUMOC', 0, iarg, 1,&
                        num, n1)
            call getvtx(' ', 'NOMOC', 0, iarg, 1,&
                        nom, n2)
            call getvtx(' ', 'NOMATR', 0, iarg, 1,&
                        nom, n3)
!           CALL LXCAPS(NOM)
            if (n1 .ne. 0) then
                call jeexin(jexnum(nomobj, num), iret)
                if (iret .eq. 0) then
                    write(iuni,*) ' L''OBJET : "',num,&
     &              '" DE LA COLLECTION : "',nomobj,'" N''EXISTE PAS'
                else
                    write(iuni,*) ' CONTENU DE L''OBJET : "',num,&
                    '" DE LA COLLECTION : "',nomobj,'"'
                    write(iuni,*) ' '
                    call jeimpa(iuni, jexnum(nomobj, num), txt)
                    call jeimpo(iuni, jexnum(nomobj, num), txt)
                    write(iuni,*) ' '
                endif
            else if (n2.ne.0) then
                call jeexin(jexnom(nomobj, nom), iret)
                if (iret .eq. 0) then
                    write(iuni,*) ' L''OBJET : "',nom,&
     &              '" DE LA COLLECTION : "',nomobj,'" N''EXISTE PAS'
                else
                    write(iuni,*) ' CONTENU DE L''OBJET : "',nom,&
                    '" DE LA COLLECTION : "',nomobj,'"'
                    write(iuni,*) ' '
                    call jeimpa(iuni, jexnom(nomobj, nom), txt)
                    call jeimpo(iuni, jexnom(nomobj, nom), txt)
                    write(iuni,*) ' '
                endif
            else if (n3.ne.0) then
                noml32 = nomobj
                call jeprat(iuni, noml32, nom(1:8), txt)
                write(iuni,*) ' '
            else
                call jelira(nomobj, 'NMAXOC', noc, k1bid)
                do 1 i = 1, noc
                    call jeexin(jexnum(nomobj, i), iret)
                    if (iret .ne. 0) then
                        write(iuni,*) ' CONTENU DE L''OBJET : "',i,&
                        '" DE LA COLLECTION : "',nomobj,'"'
                        write(iuni,*) ' '
                        call jeimpa(iuni, jexnum(nomobj, i), txt)
                        call jeimpo(iuni, jexnum(nomobj, i), txt)
                        write(iuni,*) ' '
                    endif
 1              continue
            endif
        else
            write(iuni,*) ' '
            write(iuni,*) ' CONTENU DE L''OBJET : "',nomobj,'"'
            call jeimpo(iuni, nomobj, txt)
            write(iuni,*) ' '
        endif
        write(iuni,*) ' '
        write(iuni,*) ' FIN DE L''OBJET : "',nomobj,'"'
        write(iuni,*) ' '
!
    else if (noment(1:7) .eq. 'SYSTEME') then
!
        nomcla = ' '
        call getvtx(' ', 'CLASSE', 0, iarg, 1,&
                    nomcla, n)
        call getvtx(' ', 'NOMATR', 0, iarg, 1,&
                    nom, n3)
        if (n3 .ne. 0) then
            call jeprat(iuni, '$'//nomcla(1:1), nom(1:8), txt)
            write(iuni,*) ' '
        endif
!
    else if (noment(1:8) .eq. 'ATTRIBUT') then
!
        call getvtx(' ', 'NOMOBJ', 0, iarg, 1,&
                    nomobj, n)
        call jeexin(nomobj, iret)
        write(iuni,*) ' '
        if (iret .eq. 0) then
            write(iuni,*) ' DIRECTIVE IMPR_JEVEUX '
            write(iuni,*) ' L''OBJET "',nomobj,'" N''EXISTE PAS'
            goto 9999
        else
            write(iuni,*) ' '
            write(iuni,*)' ECRITURE DES ATTRIBUTS DE "',nomobj,'"'
        endif
        write(iuni,*) ' '
        call jeimpa(iuni, nomobj, txt)
        write(iuni,*) ' '
!
    else if (noment(1:14) .eq. 'ENREGISTREMENT') then
!
        nomcla = ' '
        call getvtx(' ', 'CLASSE', 0, iarg, 1,&
                    nomcla, n)
        call getvis(' ', 'NUMERO', 0, iarg, 1,&
                    numerg, nrg)
        call getvis(' ', 'INFO', 0, iarg, 1,&
                    info, nif)
        write(iuni,*) ' '
        call jepreg(fich, nomcla, numerg, txt, info)
        write(iuni,*) ' '
!
    endif
9999  continue
end subroutine

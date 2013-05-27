subroutine mmmreg(noma, defico, resoco, depcn, ndd1,&
                  glie, glim)
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
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/mmelty.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfl.h'
    include 'asterfort/mminfm.h'
    include 'asterfort/mmnonf.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=24) :: glie, glim
    integer :: ndd1
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - POST-TRAITEMENT)
!
! CALCULER LES GLISSEMENTS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  DEPCN  : CHAM_NO_S REDUIT DE L'INCREMENT DE DEPLACEMENT CUMULE
! IN  NDD1   : NOMBRE DE DDL/NOEUD
! OUT GLIE   : GLISSEMENT ESCLAVE
! OUT GLIM   : GLISSEMENT MAITRE
!
!
!
!
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: ndimg, nzoco, ntpc
    integer :: nne, nnm, nbmae, nptm
    integer :: izone, imae, iptc, iptm, i
    integer :: nuno, ibid, jdecme
    integer :: posmae, nummae, nummam
    integer :: iconex, ilong
    real(kind=8) :: ksipc1, ksipc2, ksipr1, ksipr2
    integer :: jglie, jglim
    integer :: ztabf
    character(len=19) :: depcn
    integer :: jdepdl, jdepde
    real(kind=8) :: deplpm(3), deplpe(3)
    real(kind=8) :: tau1(3), tau2(3), ff(9)
    character(len=8) :: aliase, aliasm
    logical :: lveri
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES AU MAILLAGE
!
    call jeveuo(jexatr(noma(1:8)//'.CONNEX', 'LONCUM'), 'L', ilong)
    call jeveuo(noma(1:8)//'.CONNEX', 'L', iconex)
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
!
    ztabf = cfmmvd('ZTABF')
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(defico,'NDIM' )
    nzoco = cfdisi(defico,'NZOCO' )
    ntpc = cfdisi(defico,'NTPC' )
!
! --- ACCES AU CHAM_NO_S POUR LES DEPLACEMENTS/LAGRANGES
!
    call jeveuo(depcn(1:19)//'.CNSV', 'L', jdepde)
    call jeveuo(depcn(1:19)//'.CNSL', 'L', jdepdl)
!
! --- CREATION D OBJETS DE TRAVAIL
!      * VECTEUR DE GLISSEMENT DU NOEUD ESCLAVE
!      * VECTEUR DE GLISSEMENT DU NOEUD MAITRE
!
    call wkvect(glie, 'V V R', 2*ntpc, jglie)
    call wkvect(glim, 'V V R', 2*ntpc, jglim)
!
! --- CALCUL DU JEU AUX NOEUDS EN PRENANT LE MIN DES JEUX AUX NOEUDS,
! --- DU GLISSEMENT EN PRENANT LE MAX DU GLISSEMENT AUX NOEUDS
! --- DE L INDICATEUR DE CONTACT EN CONSIDERANT QU UN NOEUD EST EN
! --- CONTACT A PARTIR DU MOMENT OU IL L EST SUR AU MOINS UNE
! --- DES MAILLES
!
    iptc = 1
    do 10 izone = 1, nzoco
!
! --- OPTIONS SUR LA ZONE DE CONTACT
!
        lveri = mminfl(defico,'VERIF' ,izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecme = mminfi(defico,'JDECME',izone )
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        if (lveri) then
            goto 25
        endif
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do 20 imae = 1, nbmae
!
! ------- POSITION DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, defico, 'NPTM', nptm)
!
! ------- BOUCLE SUR LES POINTS
!
            do 30 iptm = 1, nptm
!
! --------- INFOS
!
                nummae = nint(zr(jtabf+ztabf*(iptc-1)+1))
                nummam = nint(zr(jtabf+ztabf*(iptc-1)+2))
                ksipr1 = zr(jtabf+ztabf*(iptc-1)+5)
                ksipr2 = zr(jtabf+ztabf*(iptc-1)+6)
                ksipc1 = zr(jtabf+ztabf*(iptc-1)+3)
                ksipc2 = zr(jtabf+ztabf*(iptc-1)+4)
!
! --------- VECTEURS DIRECTEURS DU PLAN DE CONTACT
!
                tau1(1) = zr(jtabf+ztabf*(iptc-1)+7)
                tau1(2) = zr(jtabf+ztabf*(iptc-1)+8)
                tau1(3) = zr(jtabf+ztabf*(iptc-1)+9)
                tau2(1) = zr(jtabf+ztabf*(iptc-1)+10)
                tau2(2) = zr(jtabf+ztabf*(iptc-1)+11)
                tau2(3) = zr(jtabf+ztabf*(iptc-1)+12)
!
! --------- DEPLACEMENT DU NOEUD ESCLAVE DE LA MAILLE ESCLAVE
!
                call mmelty(noma, nummae, aliase, nne, ibid)
                call mmnonf(ndimg, nne, aliase, ksipc1, ksipc2,&
                            ff)
                deplpe(1) = 0.d0
                deplpe(2) = 0.d0
                deplpe(3) = 0.d0
!
! --------- DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE
! --------- SUR LA MAILLE MAITRE POUR LE CALCUL DU GLISSEMENT
!
                do 33 i = 1, nne
                    nuno = zi(iconex+zi(ilong-1+nummae)+i-2)
                    deplpe(1) = deplpe(1)+zr(jdepde-1+ndd1*(nuno-1)+1) *ff(i)
                    deplpe(2) = deplpe(2)+zr(jdepde-1+ndd1*(nuno-1)+2) *ff(i)
                    deplpe(3) = deplpe(3)+zr(jdepde-1+ndd1*(nuno-1)+3) *ff(i)
33              continue
!
! --------- DEPLACEMENT DU NOEUD MAITRE,
! --------- PROJETE DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
!
                call mmelty(noma, nummam, aliasm, nnm, ibid)
                call mmnonf(ndimg, nnm, aliasm, ksipr1, ksipr2,&
                            ff)
                deplpm(1) = 0.d0
                deplpm(2) = 0.d0
                deplpm(3) = 0.d0
!
! --------- DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE
! --------- SUR LA MAILLE MAITRE
!
                do 40 i = 1, nnm
                    nuno = zi(iconex+zi(ilong-1+nummam)+i-2)
                    call assert(zl(jdepdl-1+ndd1*(nuno-1)+1))
                    call assert(zl(jdepdl-1+ndd1*(nuno-1)+2))
                    deplpm(1) = deplpm(1)+zr(jdepde-1+ndd1*(nuno-1)+1) *ff(i)
                    deplpm(2) = deplpm(2)+zr(jdepde-1+ndd1*(nuno-1)+2) *ff(i)
                    if (ndimg .eq. 3) then
                        call assert(zl(jdepdl-1+ndd1*(nuno-1)+3))
                        deplpm(3) = deplpm(3)+zr(jdepde-1+ndd1*(nuno- 1)+3)*ff(i)
                    endif
40              continue
!
! --------- ECRITURE DES GLISSEMENTS
!
                if (ndimg .eq. 3) then
                    zr(jglie+2*(iptc-1)) = deplpe(1)*tau1(1) + deplpe(2)*tau1(2) + deplpe(3)*tau1&
                                           &(3)
                    zr(jglie+2*(iptc-1)+1) = deplpe(1)*tau2(1) + deplpe(2)*tau2(2) + deplpe(3)*ta&
                                             &u2(3)
                    zr(jglim+2*(iptc-1)) = deplpm(1)*tau1(1) + deplpm(2)*tau1(2) + deplpm(3)*tau1&
                                           &(3)
                    zr(jglim+2*(iptc-1)+1) = deplpm(1)*tau2(1) + deplpm(2)*tau2(2) + deplpm(3)*ta&
                                             &u2(3)
                else if (ndimg.eq.2) then
                    zr(jglie+iptc-1) = deplpe(1)*tau1(1) + deplpe(2)* tau1(2)
                    zr(jglim+iptc-1) = deplpm(1)*tau1(1) + deplpm(2)* tau1(2)
                else
                    call assert(.false.)
                endif
!
! --------- LIAISON DE CONTACT SUIVANTE
!
                iptc = iptc + 1
30          continue
20      continue
25      continue
10  end do
!
    call jedema()
end subroutine

stack build
stack exec Teatrino -- --file scribble/e_TwoBuyerAll.nuscr --project
stack exec Teatrino -- --file scribble/e_TwoBuyerAll.nuscr --gen
stack exec Teatrino -- --file scribble/a_PingPongAll.nuscr --gen

stack run -- --file scribble/e_TwoBuyerAll.nuscr --refactorrole=R:Seller
stack run -- --file scribble/e_TwoBuyerAll.nuscr --refactorlabel=Quote:Price
stack run -- --file scribble/market.nuscr --relabelsite=S:B:Price:Offer
stack run -- --file scribble/e_TwoBuyerAll.nuscr --relabelsite=R:P:Quote:Price